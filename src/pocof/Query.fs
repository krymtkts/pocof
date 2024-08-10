namespace Pocof

open System
open System.Collections
open System.Management.Automation
open System.Text.RegularExpressions

open Data

module Query =
    let private equalOpt sensitive =
        match sensitive with
        | true -> StringComparison.CurrentCulture
        | _ -> StringComparison.CurrentCultureIgnoreCase

    let private likeOpt sensitive =
        match sensitive with
        | true -> WildcardOptions.None
        | _ -> WildcardOptions.IgnoreCase

    let private matchOpt sensitive =
        match sensitive with
        | true -> RegexOptions.None
        | _ -> RegexOptions.IgnoreCase

    let private equals (opt: StringComparison) (r: string) =
        match r with
        | "" -> alwaysTrue
        | _ -> String.equals opt r

    let private likes (opt: WildcardOptions) (wcp: string) =
        match wcp with
        | "" -> alwaysTrue
        | _ -> WildcardPattern.Get(wcp, opt).IsMatch

    let private matches (opt: RegexOptions) (pattern: string) =
        try
            // NOTE: expect using cache.
            Regex(pattern, opt).IsMatch
        with _ ->
            alwaysTrue

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<NoEquality>]
    type QueryPart =
        | Normal of is: (string -> bool) * tail: QueryPart
        | Property of lowerCaseName: string * is: (string -> bool) * tail: QueryPart
        | End

    let
#if !DEBUG
        inline private
#endif
        (?=>)
            (x: 'a)
            (prop: string)
            =
        try
            // TODO: consider using cache.
            let propInfo = x.GetType().GetProperty prop

            match propInfo with
            | null -> None
            | _ -> Some(propInfo.GetValue(x, null) :?> 'b)
        with _ ->
            None

    let
#if !DEBUG
        inline private
#endif
        (?->)
            (x: PSObject)
            (prop: string)
            =
        try
            let prop = x.Properties.Item prop

            match prop with
            | null -> None
            | _ -> prop.Value |> Some
        with _ ->
            None

    [<NoComparison>]
    [<NoEquality>]
    type QueryContext =
        { Queries: QueryPart
          Operator: Operator }

    [<TailCall>]
    let rec private parseQuery (is: string -> string -> bool) (acc: QueryPart) (xs: string list) =
        match xs with
        | [] -> acc
        | (x :: xs) ->
            match xs with
            | [] ->
                parseQuery is
                <| match x with
                   | Prefix ":" _ -> acc
                   | _ -> QueryPart.Normal(is x, acc)
                <| []
            | y :: zs ->
                match x with
                | Prefix ":" p -> parseQuery is <| QueryPart.Property(String.lower p, is y, acc) <| zs
                | _ -> parseQuery is <| QueryPart.Normal(is x, acc) <| xs

    let private prepareTest (state: InternalState) =
        let is =
            state.QueryCondition.CaseSensitive
            |> match state.QueryCondition.Matcher with
               | Matcher.Eq -> equalOpt >> equals
               | Matcher.Like -> likeOpt >> likes
               | Matcher.Match -> matchOpt >> matches

        match String.IsNullOrWhiteSpace state.QueryState.Query, state.QueryCondition.Invert with
        | false, true -> fun r -> is r >> not
        | _ -> is

    let private prepareQuery (state: InternalState) =
        let is = prepareTest state

        match state.QueryCondition.Operator with
        | Operator.None -> QueryPart.Normal(is state.QueryState.Query, QueryPart.End)
        | _ ->
            state.QueryState.Query
            |> String.trim
            |> String.split " "
            |> List.ofSeq
            |> parseQuery is QueryPart.End

    let private prepareNotification (state: InternalState) =
        match state.QueryCondition.Matcher with
        | Matcher.Match ->
            try
                Regex(state.QueryState.Query) |> ignore
                ""
            with e ->
                e.Message
        | _ -> ""

    let prepare (state: InternalState) =
        let queries = prepareQuery state
        let notification = prepareNotification state

        { state with
            Notification = notification },
        { Queries = queries
          Operator = state.QueryCondition.Operator }

    module InternalState =
        let prepareNotification state =
            { state with
                Notification = prepareNotification state }

    module QueryContext =
        let prepareQuery state context =
            { context with
                QueryContext.Queries = prepareQuery state }

        let prepareTest state context =
            { context with
                QueryContext.Operator = state.QueryCondition.Operator }

    let
#if !DEBUG
        inline
#endif
        private tryGetPropertyName
            (props: Generic.IReadOnlyDictionary<string, string>)
            p
            =
        match props.TryGetValue p with
        | true, n -> Some n
        | _ -> None

    let
#if !DEBUG
        inline
#endif
        private tryGetPropertyValue
            o
            =
        function
        | Some(propName) ->
            match o with
            | Entry.Dict(dct) -> dct ?=> propName
            | Entry.Obj(o) -> o ?-> propName
        | None -> None

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<NoEquality>]
    [<Struct>]
    type QueryResult =
        | End
        | Matched
        | Unmatched
        | PartialMatched
        | PropertyNotFound

    [<TailCall>]
    let rec processQueries (combination: Operator) props entry queries hasNoMatch =
        let result, tail =
            match queries with
            | QueryPart.End -> QueryResult.End, QueryPart.End
            | QueryPart.Property(p, test, tail) ->
                tryGetPropertyName props p
                |> tryGetPropertyValue entry
                |> function
                    | Some(pv) when pv.ToString() |> test -> QueryResult.Matched, tail
                    | Some(_) -> QueryResult.Unmatched, tail
                    | None -> QueryResult.PropertyNotFound, tail
            | QueryPart.Normal(test, tail) ->
                match entry with
                | Entry.Dict(dct) ->
                    match dct.Key.ToString() |> test, dct.Value.ToString() |> test with
                    | true, true -> QueryResult.Matched, tail
                    | false, false -> QueryResult.Unmatched, tail
                    | _ -> QueryResult.PartialMatched, tail
                | Entry.Obj(o) ->
                    match o.ToString() |> test with
                    | true -> QueryResult.Matched, tail
                    | _ -> QueryResult.Unmatched, tail

        match result, combination with
        | QueryResult.End, Operator.Or -> hasNoMatch
        | QueryResult.End, _ -> true
        | QueryResult.Matched, Operator.And
        | QueryResult.Unmatched, Operator.Or -> processQueries combination props entry tail false
        | QueryResult.PropertyNotFound, _ -> processQueries combination props entry tail hasNoMatch
        | _, Operator.Or -> true
        | _ -> false

    let run (context: QueryContext) (entries: Entry seq) (props: Generic.IReadOnlyDictionary<string, string>) =
        // #if DEBUG
        //         Logger.LogFile context.Queries
        // #endif

        match context.Queries with
        | QueryPart.End -> entries |> PSeq.ofSeq
        | _ ->
            let predicate (o: Entry) =
                processQueries context.Operator props o context.Queries true

            entries |> PSeq.ofSeq |> PSeq.filter predicate

    let props (state: InternalState) =
        match state.SuppressProperties, state.PropertySearch with
        | false, PropertySearch.Search(prefix: string)
        | false, PropertySearch.Rotate(prefix: string, _, _) ->
            let transform (x: string) =
                match state.QueryCondition.CaseSensitive with
                | true -> x
                | _ -> String.lower x

            let p = transform prefix
            let ret = Seq.filter (transform >> String.startsWith p) state.Properties

            match ret |> Seq.length with
            | 0 -> Error "Property not found"
            | _ -> ret |> List.ofSeq |> Ok
        | _ -> Ok <| []
