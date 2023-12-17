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

    // TODO: implement it.
    // let prepare (state: InternalState)(props: Map<string, string>) =
    //     ()

    // TODO: move returning state to prepare function.
    [<TailCall>]
    let rec private parseQuery (acc: Query list) (xs: string list) =
        // TODO: state.QueryState.Operator is NONE.
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

    let run (state: InternalState) (entries: Entry list) (props: Map<string, string>) =
        let queries =
            state.Query.Trim()
            |> String.split " "
            |> List.ofSeq
            |> parseQuery []

#if DEBUG
        Logger.logFile queries
#endif

        let values (o: Entry) =
            queries
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

        let test =
            match state.QueryState.Operator with
            | OR -> List.exists
            | _ -> List.forall

        let is =
            match state.QueryState.Matcher with
            | EQ -> equals << equalOpt
            | LIKE -> likes << likeOpt
            | MATCH -> matches << matchOpt
            <| state.QueryState.CaseSensitive

        let answer =
            match String.IsNullOrWhiteSpace state.Query, state.QueryState.Invert with
            | false, true -> not
            | _ -> id

        let predicate (o: Entry) =
            match values o with
            | [] -> true
            | xs -> xs |> test (fun x -> x |> swap ||> is |> answer)

        let notification =
            match state.QueryState.Matcher with
            | MATCH ->
                try
                    new Regex(state.Query) |> ignore
                    ""
                with
                | e -> e.Message
            | _ -> ""

        { state with Notification = notification }, entries |> List.filter predicate

    let props (state: InternalState) (entries: string list) =
        let transform (x: string) =
            match state.QueryState.CaseSensitive with
            | true -> x
            | _ -> String.lower x

        match state.PropertySearch with
        | Search (prefix: string) ->
            let p = transform prefix
            let ret = List.filter (transform >> String.startsWith p) entries

            match ret with
            | [] -> Error "Property not found"
            | _ -> Ok ret
        | _ -> Ok []
