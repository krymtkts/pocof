module PocofQuery

open Xunit
open FsUnitTyped
open pocof
open System.Management.Automation

let initState () : PocofData.InternalState =
    { Query = ""
      QueryState =
        { Matcher = PocofData.Matcher.MATCH
          Operator = PocofData.Operator.OR
          CaseSensitive = false
          Invert = false }
      PropertySearch = PocofData.PropertySearch.NonSearch
      Notification = ""
      SuppressProperties = false }

let state = initState ()

module prepare =
    ()

module props =
    let entries = [ "Name"; "Attribute"; "Length" ]

    [<Fact>]
    let ``should returns OK with emtpy list.`` () =
        PocofQuery.props { state with PropertySearch = PocofData.PropertySearch.NonSearch } entries
        |> shouldEqual (Ok [])

    [<Fact>]
    let ``should returns Error with 'Property not found'.`` () =
        PocofQuery.props { state with PropertySearch = PocofData.PropertySearch.Search "No" } entries
        |> shouldEqual (Error "Property not found")

    [<Fact>]
    let ``should returns Ok with filtered properties.`` () =
        PocofQuery.props { state with PropertySearch = PocofData.PropertySearch.Search "Na" } entries
        |> shouldEqual (Ok [ "Name" ])

module run =
    let duplicatCase (s: string) = [ s; s.ToLower() ]
    let mapToObj = List.map (PSObject.AsPSObject >> PocofData.Obj)

    let genList s =
        List.map duplicatCase s |> List.concat |> mapToObj

    let entries =
        genList [ "Name"
                  "Attribute"
                  "Length" ]

    let props = Map [ ("length", "Length") ]

    let matcher m (s: PocofData.InternalState) =
        { s with QueryState = { s.QueryState with Matcher = m } }

    let query q (s: PocofData.InternalState) = { s with Query = q }

    let caseSensitive (s: PocofData.InternalState) =
        { s with QueryState = { s.QueryState with CaseSensitive = true } }

    let invert (s: PocofData.InternalState) =
        { s with QueryState = { s.QueryState with Invert = true } }

    let opAnd (s: PocofData.InternalState) =
        { s with QueryState = { s.QueryState with Operator = PocofData.AND } }

    [<Fact>]
    let ``should returns empty if entry list is empty.`` () =
        PocofQuery.run state [] props
        |> shouldEqual (state, [])

    module ``with MATCH`` =
        let state = state |> matcher PocofData.MATCH |> query "a"

        [<Fact>]
        let ``should returns all entries if query is empty.`` () =
            let state = initState () |> matcher PocofData.MATCH

            PocofQuery.run state entries props
            |> shouldEqual (state, entries)

        [<Fact>]
        let ``should returns all entries if query is invalid pattern.`` () =
            let state = state |> query "+"

            PocofQuery.run state entries props
            |> shouldEqual (
                { state with Notification = "Invalid pattern '+' at offset 1. Quantifier {x,y} following nothing." },
                entries
            )

        [<Fact>]
        let ``should returns filtered entries.`` () =
            PocofQuery.run state entries props
            |> shouldEqual (state, genList [ "Name"; "Attribute" ])

        [<Fact>]
        let ``should returns filtered entries when matcher is match and case sensitive.`` () =
            let state = caseSensitive state

            PocofQuery.run state entries props
            |> shouldEqual (state, mapToObj [ "Name"; "name"; "attribute" ])

        [<Fact>]
        let ``should returns filtered entries when matcher is match and invert result.`` () =
            let state = invert state

            PocofQuery.run state entries props
            |> shouldEqual (state, genList [ "Length" ])

        [<Fact>]
        let ``should returns filtered entries when composite query with or operator.`` () =
            let state = state |> query "a N"

            PocofQuery.run state entries props
            |> shouldEqual (state, entries)

        [<Fact>]
        let ``should returns filtered entries when composite query with and operator.`` () =
            let state = state |> query "a N" |> opAnd

            PocofQuery.run state entries props
            |> shouldEqual (state, genList [ "Name" ])

    module ``with LIKE`` =
        let state = state |> matcher PocofData.LIKE |> query "a*"

        [<Fact>]
        let ``should returns all entries if query is empty.`` () =
            let state = initState () |> matcher PocofData.LIKE

            PocofQuery.run state entries props
            |> shouldEqual (state, entries)

        [<Fact>]
        let ``should returns matched entries when matcher is like .`` () =
            PocofQuery.run state entries props
            |> shouldEqual (state, genList [ "Attribute" ])

        [<Fact>]
        let ``should returns matched entries when matcher is like and case sensitive.`` () =
            let state = caseSensitive state

            PocofQuery.run state entries props
            |> shouldEqual (state, mapToObj [ "attribute" ])

        [<Fact>]
        let ``should returns filtered entries when matcher is like and invert result.`` () =
            let state = invert state

            PocofQuery.run state entries props
            |> shouldEqual (state, genList [ "Name"; "Length" ])

        [<Fact>]
        let ``should returns filtered entries when composite query with or operator.`` () =
            let state = state |> query "*e* N*"

            PocofQuery.run state entries props
            |> shouldEqual (state, entries)

        [<Fact>]
        let ``should returns filtered entries when composite query with and operator.`` () =
            let state = state |> query "*e* N*" |> opAnd

            PocofQuery.run state entries props
            |> shouldEqual (state, genList [ "Name" ])

    module ``with EQ`` =
        let state = state |> matcher PocofData.EQ |> query "Name"

        [<Fact>]
        let ``should returns all entries if query is empty.`` () =
            let state = initState () |> matcher PocofData.EQ

            PocofQuery.run state entries props
            |> shouldEqual (state, entries)

        [<Fact>]
        let ``should returns matched entries when matcher is eq .`` () =
            PocofQuery.run state entries props
            |> shouldEqual (state, genList [ "Name" ])

        [<Fact>]
        let ``should returns matched entries when matcher is eq and case sensitive.`` () =
            let state = caseSensitive state

            PocofQuery.run state entries props
            |> shouldEqual (state, mapToObj [ "Name" ])

        [<Fact>]
        let ``should returns filtered entries when matcher is eq and invert result.`` () =
            let state = invert state

            PocofQuery.run state entries props
            |> shouldEqual (state, genList [ "Attribute"; "Length" ])

        [<Fact>]
        let ``should returns filtered entries when composite query with or operator.`` () =
            let state = state |> query "Name Length"

            PocofQuery.run state entries props
            |> shouldEqual (state, genList [ "Name"; "Length" ])

        [<Fact>]
        let ``should returns filtered entries when composite query with and operator.`` () =
            let state = state |> query "Name Length" |> opAnd

            PocofQuery.run state entries props
            |> shouldEqual (state, List.Empty)

    module ``and Dictionary`` =
        open System.Collections
        let mapToDict = List.map (PocofData.Dict)

        let entries =
            mapToDict [ DictionaryEntry("John", "Doe")
                        DictionaryEntry("Jane", "Doe")
                        DictionaryEntry("Ming", "Wu")
                        DictionaryEntry("Taro", "Nanashi") ]

        [<Fact>]
        let ``should returns filtered entries when composite query with or operator.`` () =
            let state = state |> query "e"

            PocofQuery.run state entries props
            |> shouldEqual (
                state,
                mapToDict [ DictionaryEntry("John", "Doe")
                            DictionaryEntry("Jane", "Doe") ]
            )

        [<Fact>]
        let ``should returns filtered entries when composite query with and operator.`` () =
            let state = state |> query "e" |> opAnd

            PocofQuery.run state entries props
            |> shouldEqual (state, mapToDict [ DictionaryEntry("Jane", "Doe") ])

    module ``and Property`` =
        let getPsObj (f: string, l: string) =
            let ret = PSObject()
            ret.Properties.Add(PSNoteProperty("FN", f))
            ret.Properties.Add(PSNoteProperty("LN", l))

            ret

        let mapToPsObj = List.map (getPsObj >> PocofData.Obj)

        let entries =
            mapToPsObj [ ("John", "Doe")
                         ("Jane", "Doe")
                         ("Ming", "Wu")
                         ("Taro", "Nanashi") ]

        [<Fact>]
        let ``should returns filtered entries when composite query with or operator.`` () =
            let state = state |> query "j"
            let filtered = [ entries.[0]; entries.[1] ]

            PocofQuery.run state entries props
            |> shouldEqual (state, filtered)

        [<Fact>]
        let ``should returns filtered entries when composite query with and operator.`` () =
            let state = state |> query "a j" |> opAnd
            let filtered = [ entries.[1] ]

            PocofQuery.run state entries props
            |> shouldEqual (state, filtered)
