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

let caseSensitive (s: PocofData.InternalState) =
    { s with QueryState.CaseSensitive = true }

module prepare =
    ()

module props =
    let entries = [ "Name"; "Attribute"; "Length" ]

    [<Fact>]
    let ``should returns OK with empty list.`` () =
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

    [<Fact>]
    let ``should returns Ok with filtered properties when case sensitive.`` () =
        let state =
            caseSensitive { state with PropertySearch = PocofData.PropertySearch.Search "Na" }

        PocofQuery.props state entries
        |> shouldEqual (Ok [ "Name" ])

module run =
    let duplicateCase (s: string) = [ s; s.ToLower() ]
    let mapToObj = List.map (PSObject.AsPSObject >> PocofData.Obj)

    let genList s =
        List.map duplicateCase s
        |> List.concat
        |> mapToObj

    let matcher m (s: PocofData.InternalState) = { s with QueryState.Matcher = m }

    let query q (s: PocofData.InternalState) = { s with Query = q }

    let invert (s: PocofData.InternalState) = { s with QueryState.Invert = true }

    let opAnd (s: PocofData.InternalState) =
        { s with QueryState.Operator = PocofData.AND }

    module ``with a simple query`` =
        let entries =
            genList [ "Name"
                      "Attribute"
                      "Length" ]

        let props = Map [ ("length", "Length") ]

        [<Fact>]
        let ``should returns empty if entry list is empty.`` () =
            PocofQuery.run state [] props
            |> shouldEqual (state, [])

        module ``of MATCH`` =
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
                    { state with Notification = "Invalid pattern '+' at offset 1. Quantifier '+' following nothing." },
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

        module ``of LIKE`` =
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

        module ``of EQ`` =
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
                |> shouldEqual (state, [])

    module ``with a Dictionary query`` =
        let props = Map []

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

        [<Fact>]
        let ``should returns filtered entries when property query.`` () =
            let props =
                DictionaryEntry("Jane", "Doe")
                |> PSObject.AsPSObject
                |> (fun o -> o.Properties)
                |> Seq.map (fun p -> p.Name.ToLower(), p.Name)
                |> Map

            let state = state |> query ":key ja" |> opAnd

            PocofQuery.run state entries props
            |> shouldEqual (state, mapToDict [ DictionaryEntry("Jane", "Doe") ])

    module ``with a Property query`` =
        let getPsObj (f: string, l: string) =
            let ret = PSObject()

            [ "Fn", f; "Ln", l ]
            |> List.iter (fun (k, v) -> ret.Properties.Add(PSNoteProperty(k, v)))

            ret

        let mapToPsObj = List.map (getPsObj >> PocofData.Obj)

        let entries =
            mapToPsObj [ ("John", "Doe")
                         ("Jane", "Doe")
                         ("Ming", "Wu")
                         ("Taro", "Nanashi") ]

        let props = Map [ "fn", "Fn"; "ln", "Ln" ]

        [<Fact>]
        let ``should returns filtered entries when composite query with or operator.`` () =
            let state = state |> query ":fn a :ln d"

            let filtered =
                [ entries.[0]
                  entries.[1]
                  entries.[3] ]

            PocofQuery.run state entries props
            |> shouldEqual (state, filtered)

        [<Fact>]
        let ``should returns filtered entries when composite query with and operator.`` () =
            let state = state |> query ":fn a :ln d" |> opAnd
            let filtered = [ entries.[1] ]

            PocofQuery.run state entries props
            |> shouldEqual (state, filtered)

        [<Fact>]
        let ``should returns all entries when property not exists.`` () =
            let state = state |> query ":f a"

            PocofQuery.run state entries props
            |> shouldEqual (state, entries)

        [<Fact>]
        let ``should returns all entries when incomplete composite query.`` () =
            let state = state |> query ":fn "

            PocofQuery.run state entries props
            |> shouldEqual (state, entries)

        [<Fact>]
        let ``should returns filtered entries when incomplete composite query.`` () =
            let state = state |> query "a :fn"
            let filtered = [ entries.[1]; entries.[3] ]

            PocofQuery.run state entries props
            |> shouldEqual (state, filtered)
