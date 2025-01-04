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
    type QueryPart =
        | Normal of is: (string -> bool)
        | Property of name: string * is: (string -> bool)

    [<NoComparison>]
    [<NoEquality>]
    type QueryContext =
        { Queries: QueryPart list
          Operator: Operator }

    [<TailCall>]
    let rec private parseQuery (is: string -> string -> bool) (acc: QueryPart list) (xs: string list) =
        match xs with
        | [] -> acc
        | x :: xs ->
            match xs with
            | [] ->
                parseQuery is
                <| match x with
                   | Prefix ":" _ -> acc
                   | _ -> QueryPart.Normal(is x) :: acc
                <| []
            | y :: zs ->
                match x with
                | Prefix ":" p -> parseQuery is <| QueryPart.Property(p, is y) :: acc <| zs
                | _ -> parseQuery is <| QueryPart.Normal(is x) :: acc <| xs

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
        match query with
        | "" -> []
        | _ ->
            let is = prepareTest condition

            query |> String.trim |> Regex.split @"\s+" |> List.ofSeq |> parseQuery is []

    let private prepareNotification (query: string) (condition: QueryCondition) =
        match condition.Matcher with
        | Matcher.Match ->
            try
                Regex(query) |> ignore
                None
            with e ->
                e.Message |> Some
        | _ -> None

    let prepare (state: InternalState) =
        let queries = prepareQuery state.QueryState.Query state.QueryCondition
        let notification = prepareNotification state.QueryState.Query state.QueryCondition

        { state with
            Notification = notification },
        { Queries = queries
          Operator = state.QueryCondition.Operator }

    module InternalState =
        let prepareNotification state =
            { state with
                Notification = prepareNotification state.QueryState.Query state.QueryCondition }

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
            | Operator.And -> fun c acc -> (<@ c %x @>, acc, <@ false @>)
            | Operator.Or -> fun c acc -> (<@ c %x @>, <@ true @>, acc)

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
        (op: Operator)
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

                generatePredicate op props (x :: acc) tail
            | _ -> generatePredicate op props acc tail

        | QueryPart.Normal(test) :: tail ->
            let combination =
                match op with
                | Operator.And -> (&&)
                | Operator.Or -> (||)

            let x entry =
                match entry with
                | Entry.Dict(dct) -> combination (dct.Key.ToString() |> test) (dct.Value.ToString() |> test)
                | Entry.Obj(o) -> o.ToString() |> test

            generatePredicate op props (x :: acc) tail
        | [] -> generateExpr op acc

    let run (context: QueryContext) (entries: Entry pseq) (props: Generic.IReadOnlyDictionary<string, string>) =
        // #if DEBUG
        //         Logger.LogFile context.Queries
        // #endif

        match context.Queries with
        | [] -> entries
        | _ ->
            let predicate = generatePredicate context.Operator props [] context.Queries

            entries |> PSeq.filter predicate

    let props (state: InternalState) =
        match state.SuppressProperties, state.PropertySearch with
        | false, PropertySearch.Search(prefix: string)
        | false, PropertySearch.Rotate(prefix: string, _, _) ->
            let ret = state.Properties |> Seq.filter (String.startsWithIgnoreCase prefix)

            match ret |> Seq.length with
            | 0 -> Error "Property not found"
            | _ -> ret |> List.ofSeq |> Ok
        | _ -> Ok []
