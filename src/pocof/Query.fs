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

    let run (state: PocofData.InternalState) (entries: PocofData.Entry list) =
        let values (o: PocofData.Entry) =
            match o with
            | PocofData.Dict (dct) -> [ dct.Key; dct.Value ]
            | PocofData.Obj (o) -> [ o ] // TODO: refer the property if specified.
            |> List.map (fun st -> st.ToString())

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
            let queries =
                match state.QueryState.Operator with
                | PocofData.NONE -> [ state.Query ]
                | _ -> state.Query.Split(" ") |> List.ofSeq
                |> List.map is

            let test =
                if state.QueryState.Operator = PocofData.OR then
                    List.exists
                else
                    List.forall

            values o
            |> List.allPairs queries
            |> test (fun (pred, s) -> pred s |> answer)

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
            let ret = List.filter (fun (s: string) -> (transform s).StartsWith prefix) entries

            if List.isEmpty ret then
                Error "Property not found"
            else
                Ok ret
        | _ -> Ok []
