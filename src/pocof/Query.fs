namespace pocof

open System
open System.Management.Automation
open System.Text.RegularExpressions

module PocofQuery =
    let run (s: PocofData.InternalState) (l: PSObject list) =
        let (==) (r: string) l =
            match r with
            | "" -> true
            | _ ->
                if s.Filter.CaseSensitive then
                    StringComparison.CurrentCulture
                else
                    StringComparison.CurrentCultureIgnoreCase
                |> fun opt -> r.Equals(l, opt)


        let (=*=) wcp o =
            if s.Filter.CaseSensitive then
                WildcardOptions.None
            else
                WildcardOptions.IgnoreCase
            |> fun opt -> WildcardPattern.Get(wcp, opt).IsMatch(o)

        let (=~=) regex o =
            if s.Filter.CaseSensitive then
                RegexOptions.None
            else
                RegexOptions.IgnoreCase
            |> fun op ->
                try
                    new Regex(regex, op)
                with
                | _ -> new Regex(String.Empty, op)
            |> fun r -> r.IsMatch(o)

        let notification =
            match s.Filter.Matcher with
            | PocofData.MATCH ->
                try
                    new Regex(s.Query) |> ignore
                    ""
                with
                | e -> e.Message
            | _ -> ""

        let value o =
            if s.Filter.CaseSensitive then
                o.ToString() // TODO: refer the property if specified.
            else
                o.ToString().ToLower()

        let is =
            s.Query
            |> match s.Filter.Matcher with
               | PocofData.EQ -> (==)
               | PocofData.LIKE -> (=*=)
               | PocofData.MATCH -> (=~=)

        let answer a = if s.Filter.Invert then not a else a

        let predicate (o: PSObject) = value o |> is |> answer

        { s with Notification = notification },
        query {
            for o in l do
                where (predicate o)
                select o
        }
        |> List.ofSeq
