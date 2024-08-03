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

    let private equals (opt: StringComparison) (r: string, l: string) =
        match r with
        | "" -> true
        | _ -> String.equals opt r l

    let private likes (opt: WildcardOptions) (wcp: string, value: string) =
        match wcp with
        | "" -> true
        | _ -> WildcardPattern.Get(wcp, opt).IsMatch value

    let private matches (opt: RegexOptions) (pattern: string, value: string) =
        try
            // NOTE: expect using cache.
            Regex.IsMatch(value, pattern, opt)
        with _ ->
            true

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type QueryPart =
        | Normal of value: string
        | Property of lowerCaseName: string * value: string

    let
#if !DEBUG
        inline
#endif
        private (?=>)
            (x: 'a)
            (prop: string)
            =
        try
            // TODO: not so good.
            let propInfo = x.GetType().GetProperty prop
            Some(propInfo.GetValue(x, null) :?> 'b)
        with _ ->
            None

    let
#if !DEBUG
        inline
#endif
        private (?->)
            (x: PSObject)
            (prop: string)
            =
        try
            Some (x.Properties.Item prop).Value
        with _ ->
            None

    [<NoComparison>]
    [<NoEquality>]
    type QueryContext =
        { Queries: QueryPart list
          Test: Operator
          Is: string * string -> bool
          Answer: bool -> bool }

    [<TailCall>]
    let rec private parseQuery (acc: QueryPart list) (xs: string list) =
        match xs with
        | [] -> acc
        | (x :: xs) ->
            match xs with
            | [] ->
                parseQuery
                <| match x with
                   | Prefix ":" _ -> acc
                   | _ -> QueryPart.Normal x :: acc
                <| []
            | y :: zs ->
                match x with
                | Prefix ":" p -> parseQuery <| QueryPart.Property(String.lower p, y) :: acc <| zs
                | _ -> parseQuery <| QueryPart.Normal x :: acc <| xs

    let private prepareQuery (state: InternalState) =
        match state.QueryCondition.Operator with
        | Operator.None -> [ QueryPart.Normal state.QueryState.Query ]
        | _ ->
            state.QueryState.Query
            |> String.trim
            |> String.split " "
            |> List.ofSeq
            |> parseQuery []

    let private prepareIs (state: InternalState) =
        match state.QueryCondition.Matcher with
        | Matcher.Eq -> equals << equalOpt
        | Matcher.Like -> likes << likeOpt
        | Matcher.Match -> matches << matchOpt
        <| state.QueryCondition.CaseSensitive

    let private prepareAnswer (state: InternalState) =
        match String.IsNullOrWhiteSpace state.QueryState.Query, state.QueryCondition.Invert with
        | false, true -> not
        | _ -> id

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
        let is = prepareIs state
        let answer = prepareAnswer state
        let notification = prepareNotification state

        { state with
            Notification = notification },
        { Queries = queries
          Test = state.QueryCondition.Operator
          Is = is
          Answer = answer }

    module InternalState =
        let prepareNotification state =
            { state with
                Notification = prepareNotification state }

    module QueryContext =
        let prepareQuery state context =
            { context with
                Queries = prepareQuery state }

        let prepareIs state context = { context with Is = prepareIs state }

        let prepareTest state context =
            { context with
                Test = state.QueryCondition.Operator }

        let prepareAnswer state context =
            { context with
                Answer = prepareAnswer state }


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
    type QueryResult =
        | Matched
        | Unmatched
        | PartialMatched
        | PropertyNotFound

    [<TailCall>]
    let rec processQueries (test: string * string -> bool) (combination: Operator) props entry queries invalidProperty =
        match queries with
        | [] -> (combination <> Operator.Or) || invalidProperty
        | head :: tail ->
            let a =
                match head with
                | QueryPart.Property(p, v) ->
                    tryGetPropertyName props p
                    |> tryGetPropertyValue entry
                    |> function
                        | Some(pv) ->
                            match test (pv.ToString(), v) with
                            | true -> QueryResult.Matched
                            | _ -> QueryResult.Unmatched
                        | None -> QueryResult.PropertyNotFound
                | QueryPart.Normal(v) ->
                    match entry with
                    | Entry.Dict(dct) ->
                        match test (dct.Key.ToString(), v), test (dct.Value.ToString(), v) with
                        | true, true -> QueryResult.Matched
                        | false, false -> QueryResult.Unmatched
                        | _ -> QueryResult.PartialMatched
                    | Entry.Obj(o) ->
                        match test (o.ToString(), v) with
                        | true -> QueryResult.Matched
                        | _ -> QueryResult.Unmatched

            match a, combination with
            | QueryResult.Matched, Operator.And
            | QueryResult.Unmatched, Operator.Or -> processQueries test combination props entry tail invalidProperty
            | QueryResult.PropertyNotFound, _ -> processQueries test combination props entry tail true
            | _, Operator.Or -> true
            | _ -> false

    let run (context: QueryContext) (entries: Entry seq) (props: Generic.IReadOnlyDictionary<string, string>) =
#if DEBUG
        Logger.LogFile context.Queries
#endif
        // NOTE: use pipeline for inline optimization.
        let test x =
            x |> swap |> context.Is |> context.Answer

        match context.Queries with
        | [] -> entries |> PSeq.ofSeq
        | _ ->
            let predicate (o: Entry) =
                processQueries test context.Test props o context.Queries false

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
