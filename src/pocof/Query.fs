namespace pocof

open System
open System.Management.Automation
open System.Text.RegularExpressions
open System.Collections

module PocofQuery =
    let run (s: PocofData.InternalState) (l: obj list) =
        let (==) (r: string) l =
            match r with
            | "" -> true
            | _ ->
                if s.QueryState.CaseSensitive then
                    StringComparison.CurrentCulture
                else
                    StringComparison.CurrentCultureIgnoreCase
                |> fun opt -> r.Equals(l, opt)


        let (=*=) wcp o =
            if s.QueryState.CaseSensitive then
                WildcardOptions.None
            else
                WildcardOptions.IgnoreCase
            |> fun opt -> WildcardPattern.Get(wcp, opt).IsMatch(o)

        let (=~=) regex o =
            if s.QueryState.CaseSensitive then
                RegexOptions.None
            else
                RegexOptions.IgnoreCase
            |> fun op ->
                try
                    new Regex(regex, op)
                with
                | _ -> new Regex(String.Empty, op)
            |> fun r -> r.IsMatch(o)

        let values (o: obj) =
            let strs =
                match o with
                | :? DictionaryEntry as dct -> [ dct.Key; dct.Value ]
                | _ as o -> [ o ] // TODO: refer the property if specified.

            strs
            |> List.map (fun st ->
                if s.QueryState.CaseSensitive then
                    st.ToString()
                else
                    st.ToString().ToLower())

        let is =
            s.Query
            |> match s.QueryState.Matcher with
               | PocofData.EQ -> (==)
               | PocofData.LIKE -> (=*=)
               | PocofData.MATCH -> (=~=)

        let answer a =
            if s.QueryState.Invert then not a else a

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
