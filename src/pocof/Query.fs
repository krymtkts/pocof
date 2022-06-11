namespace pocof

open System
open System.Management.Automation
open System.Text.RegularExpressions

module PocofQuery =
    let run (s: PocofData.InternalState) (l: PSObject list) =
        let (==) r l =
            let opt =
                if s.Filter.CaseSensitive then
                    StringComparison.CurrentCulture
                else
                    StringComparison.CurrentCultureIgnoreCase

            if r = String.Empty then
                r.Equals(l, opt)
            else
                true

        let (=*=) wcp o =
            let opt =
                if s.Filter.CaseSensitive then
                    WildcardOptions.None
                else
                    WildcardOptions.IgnoreCase

            WildcardPattern.Get(wcp, opt).IsMatch(o)

        let (=~=) regex o =
            let op =
                if s.Filter.CaseSensitive then
                    RegexOptions.None
                else
                    RegexOptions.IgnoreCase

            let r =
                try
                    new Regex(regex, op)
                with
                | _ -> new Regex(String.Empty, op)

            r.IsMatch(o)

        let value o =
            if s.Filter.CaseSensitive then
                o.ToString() // TODO: refer the property if specified.
            else
                o.ToString().ToLower()

        let is =
            let op =
                match s.Filter.Matcher with
                | PocofData.EQ -> (==)
                | PocofData.LIKE -> (=*=)
                | PocofData.MATCH -> (=~=)

            op s.Query

        let answer a = if s.Filter.Invert then not a else a

        let predicate (o: PSObject) = value o |> is |> answer

        query {
            for o in l do
                where (predicate o)
                select o
        }
        |> List.ofSeq
