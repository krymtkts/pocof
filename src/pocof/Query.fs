namespace Pocof

open System
open System.Collections
open System.Management.Automation
open System.Text.RegularExpressions

open Operator
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
        | Normal of is: (string -> bool) * tail: QueryPart
        | Property of lowerCaseName: string * is: (string -> bool) * tail: QueryPart
        | End

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
        | "" -> QueryPart.End
        | _ ->
            let is = prepareTest condition

            query
            |> String.trim
            |> String.split " "
            |> List.ofSeq
            |> parseQuery is QueryPart.End

    let private prepareNotification (query: string) (condition: QueryCondition) =
        match condition.Matcher with
        | Matcher.Match ->
            try
                Regex(query) |> ignore
                ""
            with e ->
                e.Message
        | _ -> ""

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
        private getPropertyValue
            propName
            o
            =
        match o with
        | Entry.Dict(dct) -> dct ?=> propName
        | Entry.Obj(o) -> o ?-> propName

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
    type private QueryResult =
        | End
        | Matched
        | Unmatched
        | PartialMatched
        | PropertyNotFound

    [<TailCall>]
    let rec private processQueries (combination: Operator) props entry queries hasNoMatch =
        let result, tail =
            match queries with
            | QueryPart.Property(p, test, tail) ->
                match tryGetPropertyName props p |> tryGetPropertyValue entry with
                | Some(pv) when pv.ToString() |> test -> QueryResult.Matched
                | Some(_) -> QueryResult.Unmatched
                | None -> QueryResult.PropertyNotFound
                , tail
            | QueryPart.Normal(test, tail) ->
                match entry with
                | Entry.Dict(dct) ->
                    match dct.Key.ToString() |> test, dct.Value.ToString() |> test with
                    | true, true -> QueryResult.Matched
                    | false, false -> QueryResult.Unmatched
                    | _ -> QueryResult.PartialMatched
                | Entry.Obj(o) ->
                    match o.ToString() |> test with
                    | true -> QueryResult.Matched
                    | _ -> QueryResult.Unmatched
                , tail
            | QueryPart.End -> QueryResult.End, QueryPart.End

        match result, combination with
        | QueryResult.Matched, Operator.And
        | QueryResult.Unmatched, Operator.Or -> processQueries combination props entry tail false
        | QueryResult.End, Operator.And
        | QueryResult.Matched, Operator.Or
        | QueryResult.PartialMatched, Operator.Or -> true
        | QueryResult.Unmatched, Operator.And
        | QueryResult.PartialMatched, Operator.And -> false
        | QueryResult.End, Operator.Or -> hasNoMatch
        | QueryResult.PropertyNotFound, _ -> processQueries combination props entry tail hasNoMatch

    [<TailCall>]
    let rec private generatePredicate (combination: bool -> bool -> bool) props (acc: (Entry -> bool) list) queries =
        match queries with
        | QueryPart.Property(p, test, tail) ->
            match tryGetPropertyName props p with
            | Some(pn) ->
                let x entry =
                    match getPropertyValue pn entry with
                    | Some(pv) -> pv.ToString() |> test
                    | None -> false

                generatePredicate combination props (x :: acc) tail
            | None -> generatePredicate combination props acc tail

        | QueryPart.Normal(test, tail) ->
            let x entry =
                match entry with
                | Entry.Dict(dct) -> combination (dct.Key.ToString() |> test) (dct.Value.ToString() |> test)
                | Entry.Obj(o) -> o.ToString() |> test

            generatePredicate combination props (x :: acc) tail
        | QueryPart.End ->
            match acc with
            | [] -> alwaysTrue
            // TODO: should i use code quotation to remove redundant lambda?
            | _ -> acc |> List.rev |> List.reduce (fun acc x -> fun e -> combination (acc e) (x e))

    let run (context: QueryContext) (entries: Entry pseq) (props: Generic.IReadOnlyDictionary<string, string>) =
        // #if DEBUG
        //         Logger.LogFile context.Queries
        // #endif

        match context.Queries with
        | QueryPart.End -> entries
        | _ ->
            let combination =
                match context.Operator with
                | Operator.And -> (&&)
                | Operator.Or -> (||)

            let predicate = generatePredicate combination props [] context.Queries

            entries |> PSeq.filter predicate

    let props (state: InternalState) =
        match state.SuppressProperties, state.PropertySearch with
        | false, PropertySearch.Search(prefix: string)
        | false, PropertySearch.Rotate(prefix: string, _, _) ->
            let ret =
                state.Properties
                |> Seq.filter (fun (s: string) -> s.StartsWith(prefix, StringComparison.CurrentCultureIgnoreCase))

            match ret |> Seq.length with
            | 0 -> Error "Property not found"
            | _ -> ret |> List.ofSeq |> Ok
        | _ -> Ok []
