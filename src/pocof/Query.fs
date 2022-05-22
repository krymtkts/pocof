namespace pocof

open System
open System.Management.Automation
open System.Text.RegularExpressions

module PocofQuery =
    let internal (=~=) regex o = Regex.IsMatch(o, regex)

    let run (s: PocofData.InternalState) (l: PSObject list) =
        let value o =
            if s.Filter.CaseSensitive then
                o.ToString()
            else
                o.ToString().ToLower()

        let is =
            match s.Filter.Matcher with
            | PocofData.EQ -> ((=) s.Query)
            | PocofData.LIKE -> ((=) s.Query) // TODO: how can i write like operator?
            | PocofData.MATCH -> ((=~=) s.Query)

        let answer a = if s.Filter.Invert then not a else a

        let predicate (o: PSObject) = value o |> is |> answer

        query {
            for o in l do
                where (predicate o)
                select o
        }
        |> List.ofSeq
