namespace pocof

open System
open System.Management.Automation
open System.Text.RegularExpressions

module PocofQuery =
    let private equalOpt sensitive =
        if sensitive then
            StringComparison.CurrentCulture
        else
            StringComparison.CurrentCultureIgnoreCase

    let private likeOpt sensitive =
        if sensitive then
            WildcardOptions.None
        else
            WildcardOptions.IgnoreCase

    let private matchOpt sensitive =
        if sensitive then
            RegexOptions.None
        else
            RegexOptions.IgnoreCase

    let private (==) opt r l =
        match r with
        | "" -> true
        | _ -> r.Equals(l, opt)

    let private (=*=) (opt: WildcardOptions) wcp o =
        match wcp with
        | "" -> true
        | _ -> WildcardPattern.Get(wcp, opt).IsMatch(o)

    let private (=~=) opt pattern o =
        try
            new Regex(pattern, opt)
        with
        | _ -> new Regex(String.Empty, opt)
        |> fun r -> r.IsMatch(o)

    type Query =
        | Normal of string
        | Property of string * string

    let inline private (/?) (x: 'a) (prop: string) =
        try
            // TODO: not so good.
            let propInfo = x.GetType().GetProperty(prop)
            Some(propInfo.GetValue(x, null) :?> 'b)
        with
        | _ -> None

    let inline private (/?/) (x: PSObject) (prop: string) =
        try
            Some((x.Properties.Item prop).Value)
        with
        | _ -> None

    let run (state: PocofData.InternalState) (entries: PocofData.Entry list) (props: Map<string, string>) =
        let rec parseQuery (acc: Query list) (xs: string list) =
            // TODO: state.QueryState.Operator is PocofData.NONE.
            match xs with
            | [] -> acc
            | (x :: xs) ->
                match xs with
                | [] ->
                    parseQuery
                    <| if x.StartsWith ":" then
                           acc
                       else
                           Normal x :: acc
                    <| []
                | y :: zs ->
                    if x.StartsWith ":" then
                        parseQuery
                        <| Property(x.[1..].ToLower(), y) :: acc
                        <| zs
                    else
                        parseQuery <| Normal x :: acc <| xs

        let queries =
            state.Query.Split [| ' ' |]
            |> List.ofSeq
            |> parseQuery []

        // PocofDebug.logFile "./debug.log" queries

        let values (o: PocofData.Entry) =
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
                            | PocofData.Dict (dct) -> dct /? pk
                            | PocofData.Obj (o) -> o /?/ pk

                        match p with
                        | Some (pv) -> (pv, v) :: acc
                        | None -> acc
                    | Normal (v) ->
                        match o with
                        | PocofData.Dict (dct) -> (dct.Key, v) :: (dct.Value, v) :: acc
                        | PocofData.Obj (o) -> (o, v) :: acc)
                []
            |> List.map (fun (s, v) -> (s.ToString(), v))

        let is q =
            match state.QueryState.Matcher with
            | PocofData.EQ -> (==) << equalOpt
            | PocofData.LIKE -> (=*=) << likeOpt
            | PocofData.MATCH -> (=~=) << matchOpt
            <| state.QueryState.CaseSensitive
            <| q

        let answer =
            if state.QueryState.Invert then
                not
            else
                id

        let predicate (o: PocofData.Entry) =
            let test =
                if state.QueryState.Operator = PocofData.OR then
                    List.exists
                else
                    List.forall

            values o |> test (fun (l, r) -> is r l |> answer)

        let notification =
            match state.QueryState.Matcher with
            | PocofData.MATCH ->
                try
                    new Regex(state.Query) |> ignore
                    ""
                with
                | e -> e.Message
            | _ -> ""

        { state with Notification = notification },
        query {
            for o in entries do
                where (predicate o)
                select o
        }
        |> List.ofSeq

    let props (state: PocofData.InternalState) (entries: string list) =
        let transform (x: string) =
            if state.QueryState.CaseSensitive then
                x
            else
                x.ToLower()

        match state.PropertySearch with
        | PocofData.Search (prefix: string) ->
            let p = transform prefix
            let ret = List.filter (fun (s: string) -> (transform s).StartsWith p) entries

            if List.isEmpty ret then
                Error "Property not found"
            else
                Ok ret
        | _ -> Ok []
