namespace Pocof

open System
open System.Collections
open System.Management.Automation
open System.Text.RegularExpressions

open Data

module Query =
    let private equals (condition: QueryCondition) =
        let cmp =
            if condition.CaseSensitive then
                StringComparison.Ordinal
            else
                StringComparison.OrdinalIgnoreCase

        if condition.Invert then
            fun (token: string) ->
                let token = token
                fun (s: string) -> s.Equals(token, cmp) |> not
        else
            fun (token: string) ->
                let token = token
                fun (s: string) -> s.Equals(token, cmp)

    let private likes (condition: QueryCondition) =
        let opt =
            if condition.CaseSensitive then
                WildcardOptions.None
            else
                WildcardOptions.IgnoreCase

        if condition.Invert then
            fun (pattern: string) ->
                let wp = WildcardPattern.Get(pattern, opt)
                fun (s: string) -> wp.IsMatch s |> not
        else
            fun (pattern: string) ->
                let wp = WildcardPattern.Get(pattern, opt)
                fun (s: string) -> wp.IsMatch s

    let private matches (condition: QueryCondition) =
        let ro =
            if condition.CaseSensitive then
                RegexOptions.None
            else
                RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant

        if condition.Invert then
            fun (pattern: string) ->
                try
                    let reg: string -> bool = Regex(pattern, ro).IsMatch
                    fun (s: string) -> reg s |> not
                with _ ->
                    alwaysTrue

        else
            fun (pattern: string) ->
                try
                    let reg: string -> bool = Regex(pattern, ro).IsMatch
                    fun (s: string) -> reg s
                with _ ->
                    alwaysTrue

    let private parseQuery (is: string -> string -> bool) (input: string) : Data.QueryPart list =
        let len = input.Length
        let mutable i = 0
        let mutable acc = []
        let mutable pendingProp: string voption = ValueNone

        let
#if !DEBUG
            inline
#endif
            skipWhitespace
                (startIndex: int)
                =
            let mutable j = startIndex

            while j < len && Char.IsWhiteSpace input[j] do
                j <- j + 1

            j

        let
#if !DEBUG
            inline
#endif
            nextToken
                (startIndex: int)
                =
            let mutable j = startIndex

            while j < len && not (Char.IsWhiteSpace input[j]) do
                j <- j + 1

            j

        while i < len do
            i <- skipWhitespace i

            if i < len then
                let start = i
                i <- nextToken i
                let tokenLen = i - start

                // NOTE: token never empty here. it is guarded by if i < len above.
                match pendingProp with
                | ValueSome prop ->
                    // NOTE: Any token after a pending property prefix becomes its value (even if it begins with ':').
                    acc <- QueryPart.Property(prop, is (input.Substring(start, tokenLen))) :: acc
                    pendingProp <- ValueNone
                | ValueNone ->
                    if input[start] = ':' then
                        // NOTE: Found a property prefix. Store (possibly empty) name; value will be bound by next token.
                        if tokenLen > 1 then
                            pendingProp <- ValueSome(input.Substring(start + 1, tokenLen - 1))
                    else
                        acc <- QueryPart.Normal(is (input.Substring(start, tokenLen))) :: acc

        acc

    let private prepareTest (condition: QueryCondition) =
        condition
        |> match condition.Matcher with
           | Matcher.Eq -> equals
           | Matcher.Like -> likes
           | Matcher.Match -> matches

    let private prepareQuery (query: string) (condition: QueryCondition) =
        match query with
        | q when q.Length = 0 -> []
        | q ->
            let is = prepareTest condition
            parseQuery is q

    let private makeCacheKey (state: InternalState) : QueryCacheKey =
        { Query = state.QueryState.Query
          Matcher = state.QueryCondition.Matcher
          CaseSensitive = state.QueryCondition.CaseSensitive
          Invert = state.QueryCondition.Invert }

    let private cacheQueries (state: InternalState) (key: QueryCacheKey) (queries: QueryPart list) : InternalState =
        { state with
            QueryCache = ({ Key = key; Queries = queries }: QueryCache) |> ValueSome }

    let prepare (state: InternalState) : struct (InternalState * QueryContext) =
        let key = makeCacheKey state

        let state, queries =
            match state.QueryCache with
            | ValueSome cache when cache.Key = key -> state, cache.Queries
            | _ ->
                let qs = prepareQuery state.QueryState.Query state.QueryCondition
                cacheQueries state key qs, qs

        struct (state,
                { Queries = queries
                  Operator = state.QueryCondition.Operator })

    module InternalState =
        let prepareNotification state =
            match state.QueryCondition.Matcher with
            | Matcher.Match ->
                try
                    Regex state.QueryState.Query |> ignore
                    ValueNone
                with e ->
                    e.Message |> ValueSome
            | _ -> ValueNone

    module QueryContext =
        let prepareQuery state context =
            let key = makeCacheKey state

            let queries =
                match state.QueryCache with
                | ValueSome cache when cache.Key = key -> cache.Queries
                | _ -> prepareQuery state.QueryState.Query state.QueryCondition

            { context with
                QueryContext.Queries = queries }

        let prepareTest state context =
            { context with
                QueryContext.Operator = state.QueryCondition.Operator }

    let private generatePredicate
        (props: Generic.IReadOnlyDictionary<string, string>)
        (op: Operator)
        (queries: QueryPart list)
        : Entry -> bool =
        let rec buildPredicates (acc: (Entry -> bool) list) queries =
            match queries with
            | QueryPart.Property(p, test) :: tail ->
                match props.TryGetValue p with
                | true, pn ->
                    let predicate (entry: Entry) =
                        match entry[pn] with
                        | null -> false
                        | pv ->
                            pv.ToString()
                            |> function
                                | null -> false
                                | sv -> test sv

                    buildPredicates (predicate :: acc) tail
                | _ -> buildPredicates acc tail

            | QueryPart.Normal test :: tail ->
                let predicate entry =
                    match entry with
                    // NOTE: A hashtable query matches either the key or the value.
                    | Entry.Dict dct ->
                        // NOTE: Dictionary keys are never null.
                        dct.Key.ToString()
                        |> function
                            | null -> false
                            | sv -> test sv
                        || match dct.Value with
                           | null -> false
                           | dv ->
                               dv.ToString()
                               |> function
                                   | null -> false
                                   | sv -> test sv
                    | Entry.Obj o -> o.ToString() |> test

                buildPredicates (predicate :: acc) tail
            | [] -> acc

        let predicates = buildPredicates [] queries |> List.toArray

        match predicates.Length with
        | 0 -> alwaysTrue
        | _ ->
            match op with
            | Operator.And ->
                fun entry ->
                    let mutable i = 0
                    let mutable result = true

                    while result && i < predicates.Length do
                        result <- predicates[i]entry
                        i <- i + 1

                    result
            | Operator.Or ->
                fun entry ->
                    let mutable i = 0
                    let mutable result = false

                    while not result && i < predicates.Length do
                        result <- predicates[i]entry
                        i <- i + 1

                    result

    let run (context: QueryContext) (entries: Entry pseq) (props: Generic.IReadOnlyDictionary<string, string>) =
        match context.Queries with
        | [] -> entries
        | _ ->
            let predicate = generatePredicate props context.Operator context.Queries
            entries |> PSeq.filter predicate

    let props (properties: string seq) (state: InternalState) =
        InternalState.prepareNotification state
        |> function
            | ValueSome message -> Error message
            | _ ->
                match state.SuppressProperties, state.PropertySearch with
                | false, PropertySearch.Search(prefix: string)
                | false, PropertySearch.Rotate(prefix: string, _) ->
                    let ret = properties |> Seq.filter (String.startsWithIgnoreCase prefix)

                    match ret |> Seq.isEmpty with
                    | true -> Error "Property not found"
                    | _ -> ret |> Ok
                | _ -> Ok Array.empty
