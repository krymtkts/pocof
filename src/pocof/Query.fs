namespace pocof

open System
open System.Management.Automation
open System.Text.RegularExpressions

open PocofData

module PocofQuery =
    let inline private equalOpt sensitive =
        match sensitive with
        | true -> StringComparison.CurrentCulture
        | _ -> StringComparison.CurrentCultureIgnoreCase

    let inline private likeOpt sensitive =
        match sensitive with
        | true -> WildcardOptions.None
        | _ -> WildcardOptions.IgnoreCase

    let inline private matchOpt sensitive =
        match sensitive with
        | true -> RegexOptions.None
        | _ -> RegexOptions.IgnoreCase

    let inline private equals (opt: StringComparison) (r: string) =
        match r with
        | "" -> alwaysTrue
        | _ -> String.equals opt r

    let inline private likes (opt: WildcardOptions) (wcp: string) =
        match wcp with
        | "" -> alwaysTrue
        | _ -> WildcardPattern.Get(wcp, opt).IsMatch

    let inline private matches (opt: RegexOptions) (pattern: string) (value: string) =
        try
            // NOTE: expect using cache.
            Regex.IsMatch(value, pattern, opt)
        with
        | _ -> true

    type Query =
        | Normal of string
        | Property of string * string

    let inline private (?=>) (x: 'a) (prop: string) =
        try
            // TODO: not so good.
            let propInfo = x.GetType().GetProperty prop
            Some(propInfo.GetValue(x, null) :?> 'b)
        with
        | _ -> None

    let inline private (?->) (x: PSObject) (prop: string) =
        try
            Some (x.Properties.Item prop).Value
        with
        | _ -> None

    type TesterType<'a> = ('a -> bool) -> list<'a> -> bool

    type QueryContext =
        { Queries: Query list
          Test: TesterType<string * string>
          Is: string -> string -> bool
          Answer: bool -> bool }

    [<TailCall>]
    let rec private parseQuery (acc: Query list) (xs: string list) =
        match xs with
        | [] -> acc
        | (x :: xs) ->
            match xs with
            | [] ->
                parseQuery
                <| match x with
                   | Prefix ":" _ -> acc
                   | _ -> Normal x :: acc
                <| []
            | y :: zs ->
                match x with
                | Prefix ":" p ->
                    parseQuery
                    <| Property(String.lower p, y) :: acc
                    <| zs
                | _ -> parseQuery <| Normal x :: acc <| xs

    let prepareQuery (state: InternalState) =
        match state.QueryCondition.Operator with
        | NONE -> [ Normal state.QueryState.Query ]
        | _ ->
            state.QueryState.Query
            |> String.trim
            |> String.split " "
            |> List.ofSeq
            |> parseQuery []

    let prepareTest (state: InternalState) =
        match state.QueryCondition.Operator with
        | OR -> List.exists
        | _ -> List.forall

    let prepareIs (state: InternalState) =
        match state.QueryCondition.Matcher with
        | EQ -> equals << equalOpt
        | LIKE -> likes << likeOpt
        | MATCH -> matches << matchOpt
        <| state.QueryCondition.CaseSensitive

    let prepareAnswer (state: InternalState) =
        match String.IsNullOrWhiteSpace state.QueryState.Query, state.QueryCondition.Invert with
        | false, true -> not
        | _ -> id

    let prepareNotification (state: InternalState) =
        match state.QueryCondition.Matcher with
        | MATCH ->
            try
                new Regex(state.QueryState.Query) |> ignore
                ""
            with
            | e -> e.Message
        | _ -> ""

    let prepare (state: InternalState) =
        let queries = prepareQuery state
        let test = prepareTest state
        let is = prepareIs state
        let answer = prepareAnswer state
        let notification = prepareNotification state

        { state with Notification = notification },
        { Queries = queries
          Test = test
          Is = is
          Answer = answer }

    let run (context: QueryContext) (entries: Entry list) (props: Map<string, string>) =
#if DEBUG
        Logger.logFile context.Queries
#endif

        let values (o: Entry) =
            context.Queries
            |> List.fold
                (fun acc x ->
                    match x with
                    | Property (k, v) ->
                        let pk =
                            match props.TryGetValue k with
                            | true, v -> v
                            | _ -> ""

                        let p =
                            match o with
                            | Dict (dct) -> dct ?=> pk
                            | Obj (o) -> o ?-> pk

                        match p with
                        | Some (pv) -> (pv, v) :: acc
                        | None -> acc
                    | Normal (v) ->
                        match o with
                        | Dict (dct) -> (dct.Key, v) :: (dct.Value, v) :: acc
                        | Obj (o) -> (o, v) :: acc)
                []
            |> List.map (fun (s, v) -> (string s, v))

        let predicate (o: Entry) =
            match values o with
            | [] -> true
            | xs ->
                xs
                |> context.Test(fun x -> x |> swap ||> context.Is |> context.Answer)

        entries |> List.filter predicate

    let props (state: InternalState) =
        let transform (x: string) =
            match state.QueryCondition.CaseSensitive with
            | true -> x
            | _ -> String.lower x

        match state.PropertySearch with
        | Search (prefix: string)
        | Rotate (prefix: string, _, _) ->
            let p = transform prefix
            let ret = List.filter (transform >> String.startsWith p) state.Properties

            match ret with
            | [] -> Error "Property not found"
            | _ -> Ok ret
        | _ -> Ok []
