// NOTE: to avoid nullness warning from LeafExpressionConverter.EvaluateQuotation in unreachable pass.
#nowarn 3264
namespace Pocof

open FSharp.Linq.RuntimeHelpers
open Microsoft.FSharp.Quotations
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
                let reg =
                    try
                        Regex(pattern, ro).IsMatch
                    with _ ->
                        alwaysTrue

                fun (s: string) -> reg s |> not
        else
            fun (pattern: string) ->
                let reg =
                    try
                        Regex(pattern, ro).IsMatch
                    with _ ->
                        alwaysTrue

                fun (s: string) -> reg s

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<NoEquality>]
    [<Struct>]
    type QueryPart =
        | Normal of is: (string -> bool)
        | Property of name: string * is: (string -> bool)

    [<NoComparison>]
    [<NoEquality>]
    [<Struct>]
    type QueryContext =
        { Queries: QueryPart list
          Operator: Operator }

    let private parseQuery (is: string -> string -> bool) (input: string) : QueryPart list =
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

    let private prepareNotification (query: string) (condition: QueryCondition) =
        match condition.Matcher with
        | Matcher.Match ->
            try
                Regex query |> ignore
                ValueNone
            with e ->
                e.Message |> ValueSome
        | _ -> ValueNone

    let prepare (state: InternalState) =
        let queries = prepareQuery state.QueryState.Query state.QueryCondition
        let notification = prepareNotification state.QueryState.Query state.QueryCondition

        struct ({ Queries = queries
                  Operator = state.QueryCondition.Operator },
                notification)

    module InternalState =
        let prepareNotification state =
            prepareNotification state.QueryState.Query state.QueryCondition

    module QueryContext =
        let prepareQuery state context =
            { context with
                QueryContext.Queries = prepareQuery state.QueryState.Query state.QueryCondition }

        let prepareTest state context =
            { context with
                QueryContext.Operator = state.QueryCondition.Operator }

    let private generateExpr
        (props: Generic.IReadOnlyDictionary<string, string>)
        (op: Operator)
        (queries: QueryPart list)
        =
        let entryVar = Var("x", typeof<Entry>)
        let entry = entryVar |> Expr.Var |> Expr.Cast<Entry>
        let falseQuote = <@ false @>
        let trueQuote = <@ true @>

        let combination =
            match op with
            | Operator.And -> fun c acc -> <@ c %entry @>, acc, falseQuote
            | Operator.Or -> fun c acc -> <@ c %entry @>, trueQuote, acc

        let rec recBody (acc: Expr<bool>) (hasCondition: bool) (queries: QueryPart list) =
            match queries with
            | QueryPart.Property(p, test) :: tail ->
                match props.TryGetValue p with
                | true, pn ->
                    let x (entry: Entry) =
                        match entry[pn] with
                        | null -> false
                        | pv -> pv.ToString() |> test

                    let acc = combination x acc |> Expr.IfThenElse |> Expr.Cast<bool>
                    recBody acc true tail
                | _ -> recBody acc hasCondition tail
            | QueryPart.Normal test :: tail ->
                let x entry =
                    match entry with
                    // NOTE: A hashtable query matches either the key or the value.
                    | Entry.Dict dct ->
                        // NOTE: Dictionary keys are never null.
                        dct.Key.ToString() |> test
                        || match dct.Value with
                           | null -> false
                           | dv -> dv.ToString() |> test
                    | Entry.Obj o -> o.ToString() |> test

                let acc = combination x acc |> Expr.IfThenElse |> Expr.Cast<bool>

                recBody acc true tail
            | [] -> struct (acc, hasCondition)

        let struct (body, hasCondition) =
            // NOTE: condition's order is already reversed.
            match queries with
            | [] -> trueQuote, false
            | queries ->
                let init =
                    match op with
                    | Operator.And -> trueQuote
                    | Operator.Or -> falseQuote

                recBody init false queries

        if hasCondition then
            Expr.Lambda(entryVar, body) |> LeafExpressionConverter.EvaluateQuotation :?> Entry -> bool
        else
            alwaysTrue

    let run (context: QueryContext) (entries: Entry pseq) (props: Generic.IReadOnlyDictionary<string, string>) =
        // #if DEBUG
        //         Logger.LogFile context.Queries
        // #endif

        match context.Queries with
        | [] -> entries
        | _ ->
            let predicate = generateExpr props context.Operator context.Queries
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
