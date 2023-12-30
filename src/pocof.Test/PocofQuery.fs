module PocofQuery

open Xunit
open FsUnitTyped
open pocof
open System.Management.Automation

let initState () : PocofData.InternalState =
    { QueryState = { Query = ""; Cursor = 0 }
      QueryCondition =
        { Matcher = PocofData.Matcher.MATCH
          Operator = PocofData.Operator.OR
          CaseSensitive = false
          Invert = false }
      PropertySearch = PocofData.PropertySearch.NoSearch
      Notification = ""
      SuppressProperties = false
      Properties =  [ "Name"; "Attribute"; "Length" ]
      Refresh = PocofData.Required }

let state = initState ()

let caseSensitive (s: PocofData.InternalState) =
    { s with QueryCondition.CaseSensitive = true }

module prepare =
    ()

module props =
    [<Fact>]
    let ``should returns OK with empty list.`` () =
        PocofQuery.props { state with PropertySearch = PocofData.PropertySearch.NoSearch }
        |> shouldEqual (Ok [])

    [<Fact>]
    let ``should returns Error with 'Property not found'.`` () =
        PocofQuery.props { state with PropertySearch = PocofData.PropertySearch.Search "No" }
        |> shouldEqual (Error "Property not found")

    [<Fact>]
    let ``should returns Ok with non filtered properties.`` () =
        PocofQuery.props { state with PropertySearch = PocofData.PropertySearch.Search "" }
        |> shouldEqual (Ok ["Name"; "Attribute"; "Length"])

    [<Fact>]
    let ``should returns Ok with filtered properties.`` () =
        PocofQuery.props { state with PropertySearch = PocofData.PropertySearch.Search "Na" }
        |> shouldEqual (Ok [ "Name" ])

    [<Fact>]
    let ``should returns Ok with filtered properties when case sensitive.`` () =
        let state =
            caseSensitive { state with PropertySearch = PocofData.PropertySearch.Search "Na" }

        PocofQuery.props state
        |> shouldEqual (Ok [ "Name" ])

    [<Fact>]
    let ``should returns Ok with non filtered properties when rotate.`` () =
        PocofQuery.props { state with PropertySearch = PocofData.PropertySearch.Rotate ("", 0, ["Name"]) }
        |> shouldEqual (Ok ["Name"; "Attribute"; "Length"])

    [<Fact>]
    let ``should returns Ok with filtered properties when rotate.`` () =
        PocofQuery.props { state with PropertySearch = PocofData.PropertySearch.Rotate ("Na", 0, ["Name"]) }
        |> shouldEqual (Ok [ "Name" ])

module run =
    let duplicateCase (s: string) = [ s; s.ToLower() ]
    let mapToObj = List.map (PSObject.AsPSObject >> PocofData.Obj)

    let genList s =
        List.map duplicateCase s
        |> List.concat
        |> mapToObj

    let matcher m (s: PocofData.InternalState) = { s with QueryCondition.Matcher = m }

    let query q (s: PocofData.InternalState) = { s with QueryState.Query = q }

    let invert (s: PocofData.InternalState) = { s with QueryCondition.Invert = true }

    let opAnd (s: PocofData.InternalState) =
        { s with QueryCondition.Operator = PocofData.AND }

    module ``with a simple query`` =
        let entries =
            genList [ "Name"
                      "Attribute"
                      "Length" ]

        let props = Map [ ("length", "Length") ]

        [<Fact>]
        let ``should returns empty if entry list is empty.`` () =
            let _, context = PocofQuery.prepare state
            PocofQuery.run context [] props
            |> shouldEqual []

        module ``of MATCH`` =
            let state = state |> matcher PocofData.MATCH |> query "a"

            [<Fact>]
            let ``should returns all entries if query is empty.`` () =
                let state = initState () |> matcher PocofData.MATCH
                let _, context = PocofQuery.prepare state

                PocofQuery.run context entries props
                |> shouldEqual entries

            [<Fact>]
            let ``should returns all entries if query is invalid pattern.`` () =
                let state = state |> query "+"
                let _, context = PocofQuery.prepare state
                PocofQuery.run context entries props
                |> shouldEqual entries

            [<Fact>]
            let ``should returns filtered entries.`` () =
                let _, context = PocofQuery.prepare state
                PocofQuery.run context entries props
                |> shouldEqual ( genList [ "Name"; "Attribute" ])

            [<Fact>]
            let ``should returns filtered entries when matcher is match and case sensitive.`` () =
                let state = caseSensitive state
                let _, context = PocofQuery.prepare state

                PocofQuery.run context entries props
                |> shouldEqual ( mapToObj [ "Name"; "name"; "attribute" ])

            [<Fact>]
            let ``should returns filtered entries when matcher is match and invert result.`` () =
                let state = invert state
                let _, context = PocofQuery.prepare state

                PocofQuery.run context entries props
                |> shouldEqual ( genList [ "Length" ])

            [<Fact>]
            let ``should returns filtered entries when composite query with or operator.`` () =
                let state = state |> query "a N"
                let _, context = PocofQuery.prepare state

                PocofQuery.run context entries props
                |> shouldEqual entries

            [<Fact>]
            let ``should returns filtered entries when composite query with and operator.`` () =
                let state = state |> query "a N" |> opAnd
                let _, context = PocofQuery.prepare state

                PocofQuery.run context entries props
                |> shouldEqual ( genList [ "Name" ])

        module ``of LIKE`` =
            let state = state |> matcher PocofData.LIKE |> query "a*"

            [<Fact>]
            let ``should returns all entries if query is empty.`` () =
                let state = initState () |> matcher PocofData.LIKE
                let _, context = PocofQuery.prepare state

                PocofQuery.run context entries props
                |> shouldEqual entries

            [<Fact>]
            let ``should returns matched entries when matcher is like .`` () =
                let _, context = PocofQuery.prepare state

                PocofQuery.run context entries props
                |> shouldEqual ( genList [ "Attribute" ])

            [<Fact>]
            let ``should returns matched entries when matcher is like and case sensitive.`` () =
                let state = caseSensitive state
                let _, context = PocofQuery.prepare state


                PocofQuery.run context entries props
                |> shouldEqual ( mapToObj [ "attribute" ])

            [<Fact>]
            let ``should returns filtered entries when matcher is like and invert result.`` () =
                let state = invert state
                let _, context = PocofQuery.prepare state


                PocofQuery.run context entries props
                |> shouldEqual ( genList [ "Name"; "Length" ])

            [<Fact>]
            let ``should returns filtered entries when composite query with or operator.`` () =
                let state = state |> query "*e* N*"
                let _, context = PocofQuery.prepare state

                PocofQuery.run context entries props
                |> shouldEqual entries

            [<Fact>]
            let ``should returns filtered entries when composite query with and operator.`` () =
                let state = state |> query "*e* N*" |> opAnd
                let _, context = PocofQuery.prepare state

                PocofQuery.run context entries props
                |> shouldEqual ( genList [ "Name" ])

        module ``of EQ`` =
            let state = state |> matcher PocofData.EQ |> query "Name"

            [<Fact>]
            let ``should returns all entries if query is empty.`` () =
                let state = initState () |> matcher PocofData.EQ
                let _, context = PocofQuery.prepare state

                PocofQuery.run context entries props
                |> shouldEqual entries

            [<Fact>]
            let ``should returns matched entries when matcher is eq .`` () =
                let _, context = PocofQuery.prepare state

                PocofQuery.run context entries props
                |> shouldEqual ( genList [ "Name" ])

            [<Fact>]
            let ``should returns matched entries when matcher is eq and case sensitive.`` () =
                let state = caseSensitive state
                let _, context = PocofQuery.prepare state

                PocofQuery.run context entries props
                |> shouldEqual ( mapToObj [ "Name" ])

            [<Fact>]
            let ``should returns filtered entries when matcher is eq and invert result.`` () =
                let state = invert state
                let _, context = PocofQuery.prepare state

                PocofQuery.run context entries props
                |> shouldEqual ( genList [ "Attribute"; "Length" ])

            [<Fact>]
            let ``should returns filtered entries when composite query with or operator.`` () =
                let state = state |> query "Name Length"
                let _, context = PocofQuery.prepare state

                PocofQuery.run context entries props
                |> shouldEqual ( genList [ "Name"; "Length" ])

            [<Fact>]
            let ``should returns filtered entries when composite query with and operator.`` () =
                let state = state |> query "Name Length" |> opAnd
                let _, context = PocofQuery.prepare state

                PocofQuery.run context entries props
                |> shouldEqual []

    module ``with a Dictionary query`` =
        let props = Map []

        open System.Collections
        let mapToDict = List.map PocofData.Dict

        let entries =
            mapToDict [ DictionaryEntry("John", "Doe")
                        DictionaryEntry("Jane", "Doe")
                        DictionaryEntry("Ming", "Wu")
                        DictionaryEntry("Taro", "Nanashi") ]

        [<Fact>]
        let ``should returns filtered entries when composite query with or operator.`` () =
            let state = state |> query "e"
            let _, context = PocofQuery.prepare state

            PocofQuery.run context entries props
            |> shouldEqual (
                mapToDict [ DictionaryEntry("John", "Doe")
                            DictionaryEntry("Jane", "Doe") ]
            )

        [<Fact>]
        let ``should returns filtered entries when composite query with and operator.`` () =
            let state = state |> query "e" |> opAnd
            let _, context = PocofQuery.prepare state

            PocofQuery.run context entries props
            |> shouldEqual ( mapToDict [ DictionaryEntry("Jane", "Doe") ])

        [<Fact>]
        let ``should returns filtered entries when property query.`` () =
            let props =
                DictionaryEntry("Jane", "Doe")
                |> PSObject.AsPSObject
                |> _.Properties
                |> Seq.map (fun p -> p.Name.ToLower(), p.Name)
                |> Map

            let state = state |> query ":key ja" |> opAnd
            let _, context = PocofQuery.prepare state

            PocofQuery.run context entries props
            |> shouldEqual (mapToDict [ DictionaryEntry("Jane", "Doe") ])

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
            let _, context = PocofQuery.prepare state

            let filtered =
                [ entries.[0]
                  entries.[1]
                  entries.[3] ]

            PocofQuery.run context entries props
            |> shouldEqual filtered

        [<Fact>]
        let ``should returns filtered entries when composite query with and operator.`` () =
            let state = state |> query ":fn a :ln d" |> opAnd
            let _, context = PocofQuery.prepare state
            let filtered = [ entries.[1] ]

            PocofQuery.run context entries props
            |> shouldEqual filtered

        [<Fact>]
        let ``should returns all entries when property not exists.`` () =
            let state = state |> query ":f a"
            let _, context = PocofQuery.prepare state

            PocofQuery.run context entries props
            |> shouldEqual entries

        [<Fact>]
        let ``should returns all entries when incomplete composite query.`` () =
            let state = state |> query ":fn "
            let _, context = PocofQuery.prepare state

            PocofQuery.run context entries props
            |> shouldEqual entries

        [<Fact>]
        let ``should returns filtered entries when incomplete composite query.`` () =
            let state = state |> query "a :fn"
            let _, context = PocofQuery.prepare state
            let filtered = [ entries.[1]; entries.[3] ]

            PocofQuery.run context entries props
            |> shouldEqual filtered
