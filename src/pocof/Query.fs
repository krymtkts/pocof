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

    let private equals (opt: StringComparison) (r: string) = String.equals opt r

    let private likes (opt: WildcardOptions) (wcp: string) = WildcardPattern.Get(wcp, opt).IsMatch

    let private matches (opt: RegexOptions) (pattern: string) =
        try
            // NOTE: expect using cache.
            Regex(pattern, opt).IsMatch
        with _ ->
            alwaysTrue

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

                if tokenLen > 0 then
                    let token = input.Substring(start, tokenLen)

                    match pendingProp with
                    | ValueSome prop ->
                        // NOTE: Any token after a pending property prefix becomes its value (even if it begins with ':').
                        acc <- QueryPart.Property(prop, is token) :: acc
                        pendingProp <- ValueNone
                    | ValueNone ->
                        if token[0] = ':' then
                            // NOTE: Found a property prefix. Store (possibly empty) name; value will be bound by next token.
                            if tokenLen > 1 then
                                pendingProp <- ValueSome(token.Substring(1))
                        else
                            acc <- QueryPart.Normal(is token) :: acc

        acc

    let private prepareTest (condition: QueryCondition) =
        let is =
            condition.CaseSensitive
            |> match condition.Matcher with
               | Matcher.Eq -> equalOpt >> equals
               | Matcher.Like -> likeOpt >> likes
               | Matcher.Match -> matchOpt >> matches

        match condition.Invert with
        | true -> fun r -> is r >> not
        | _ -> is

    let private prepareQuery (query: string) (condition: QueryCondition) =
        match query |> _.Trim() with
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

    let private generateExpr (op: Operator) (conditions: (Entry -> bool) list) =
        let xVar = Var("x", typeof<Entry>)
        let x = xVar |> Expr.Var |> Expr.Cast<Entry>

        let combination =
            match op with
            | Operator.And -> fun c acc -> <@ c %x @>, acc, <@ false @>
            | Operator.Or -> fun c acc -> <@ c %x @>, <@ true @>, acc

        let rec recBody acc conditions =
            match conditions with
            | [] -> acc
            | condition :: conditions ->
                let acc = combination condition acc |> Expr.IfThenElse |> Expr.Cast<bool>
                recBody acc conditions

        let body =
            // NOTE: condition's order is already reversed.
            match conditions with
            | [] -> <@ true @>
            | condition :: conditions ->
                let term =
                    Expr.IfThenElse(<@ condition %x @>, <@ true @>, <@ false @>) |> Expr.Cast<bool>

                recBody term conditions

        let lambda = Expr.Lambda(xVar, body)

        lambda |> LeafExpressionConverter.EvaluateQuotation :?> Entry -> bool

    [<TailCall>]
    let rec private generatePredicate
        (props: Generic.IReadOnlyDictionary<string, string>)
        (acc: (Entry -> bool) list)
        queries
        =
        match queries with
        | QueryPart.Property(p, test) :: tail ->
            match props.TryGetValue p with
            | true, pn ->
                let x (entry: Entry) =
                    match entry[pn] with
                    | null -> false
                    | pv -> pv.ToString() |> test

                generatePredicate props (x :: acc) tail
            | _ -> generatePredicate props acc tail

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

            generatePredicate props (x :: acc) tail
        | [] -> acc

    let run (context: QueryContext) (entries: Entry pseq) (props: Generic.IReadOnlyDictionary<string, string>) =
        // #if DEBUG
        //         Logger.LogFile context.Queries
        // #endif

        match context.Queries with
        | [] -> entries
        | _ ->
            let predicate =
                generatePredicate props [] context.Queries |> generateExpr context.Operator

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
