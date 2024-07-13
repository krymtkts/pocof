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
        | "" -> alwaysTrue ()
        | _ -> String.equals opt r l

    let private likes (opt: WildcardOptions) (wcp: string, value: string) =
        match wcp with
        | "" -> alwaysTrue ()
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

    type TesterType<'A> = ('A -> bool) -> 'A list -> bool

    [<NoComparison>]
    [<NoEquality>]
    type QueryContext =
        { Queries: QueryPart list
          Test: TesterType<string * string>
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

    let private prepareTest (state: InternalState) =
        match state.QueryCondition.Operator with
        | Operator.Or -> List.exists
        | _ -> List.forall

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
        let test = prepareTest state
        let is = prepareIs state
        let answer = prepareAnswer state
        let notification = prepareNotification state

        { state with
            Notification = notification },
        { Queries = queries
          Test = test
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
                Test = prepareTest state }

        let prepareAnswer state context =
            { context with
                Answer = prepareAnswer state }

    let run (context: QueryContext) (entries: Entry seq) (props: Generic.IReadOnlyDictionary<string, string>) =
#if DEBUG
        Logger.LogFile context.Queries
#endif

        let values (o: Entry) =
            context.Queries
            |> List.fold
                (fun acc x ->
                    match x with
                    | QueryPart.Property(p, v) ->
                        let propName =
                            match props.TryGetValue p with
                            | true, v -> v
                            | _ -> ""

                        let prop =
                            match o with
                            | Entry.Dict(dct) -> dct ?=> propName
                            | Entry.Obj(o) -> o ?-> propName

                        match prop with
                        | Some(pv) -> (pv, v) :: acc
                        | None -> acc
                    | QueryPart.Normal(v) ->
                        match o with
                        | Entry.Dict(dct) -> (dct.Key, v) :: (dct.Value, v) :: acc
                        | Entry.Obj(o) -> (o, v) :: acc)
                []
            // NOTE: stringify using the current locale.
            |> List.map (fun (s, v) -> (s.ToString(), v))

        let predicate (o: Entry) =
            match values o with
            | [] -> true
            | xs -> xs |> context.Test(fun x -> x |> swap |> (context.Is >> context.Answer))

        entries |> Seq.filter predicate

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
