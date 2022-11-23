namespace pocof

open System
open System.Management.Automation
open System.Text.RegularExpressions
open System.Collections

module PocofQuery =
    let equalOpt sensitive =
        if sensitive then
            StringComparison.CurrentCulture
        else
            StringComparison.CurrentCultureIgnoreCase

    let likeOpt sensitive =
        if sensitive then
            WildcardOptions.None
        else
            WildcardOptions.IgnoreCase

    let matchOpt sensitive =
        if sensitive then
            RegexOptions.None
        else
            RegexOptions.IgnoreCase

    let (==) opt r l =
        match r with
        | "" -> true
        | _ -> r.Equals(l, opt)

    let (=*=) (opt: WildcardOptions) wcp o =
        match wcp with
        | "" -> true
        | _ -> WildcardPattern.Get(wcp, opt).IsMatch(o)

    let (=~=) opt regex o =
        try
            new Regex(regex, opt)
        with
        | _ -> new Regex(String.Empty, opt)
        |> fun r -> r.IsMatch(o)

    let run (s: PocofData.InternalState) (l: obj list) =
        let values (o: obj) =
            match o with
            | :? DictionaryEntry as dct -> [ dct.Key; dct.Value ]
            | _ as o -> [ o ] // TODO: refer the property if specified.
            |> List.map (fun st -> st.ToString())

        let is =
            match s.QueryState.Matcher with
            | PocofData.EQ -> (==) << equalOpt
            | PocofData.LIKE -> (=*=) << likeOpt
            | PocofData.MATCH -> (=~=) << matchOpt
            <| s.QueryState.CaseSensitive
            <| s.Query

        let answer = if s.QueryState.Invert then not else id

        let predicate (o: obj) = values o |> List.exists is |> answer

        let notification =
            match s.QueryState.Matcher with
            | PocofData.MATCH ->
                try
                    new Regex(s.Query) |> ignore
                    ""
                with
                | e -> e.Message
            | _ -> ""

        { s with Notification = notification },
        query {
            for o in l do
                where (predicate o)
                select o
        }
        |> List.ofSeq
