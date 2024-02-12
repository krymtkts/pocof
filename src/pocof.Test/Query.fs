module PocofTest.Query

open System.Management.Automation

open Xunit
open FsUnitTyped

open Pocof

let initState () : Data.InternalState =
    { QueryState = {
        Query = ""
        Cursor = 0
        WindowBeginningCursor = 0
        WindowWidth = 0}
      QueryCondition =
        { Matcher = Data.Matcher.Match
          Operator = Data.Operator.Or
          CaseSensitive = false
          Invert = false }
      PropertySearch = Data.PropertySearch.NoSearch
      Notification = ""
      SuppressProperties = false
      Properties =  [ "Name"; "Attribute"; "Length" ]
      Prompt = "query"
      FilteredCount = 0
      ConsoleWidth = 60
      Refresh = Data.Refresh.Required }

let state = initState ()

let caseSensitive (s: Data.InternalState) =
    { s with QueryCondition.CaseSensitive = true }

module prepare =
    ()

module props =
    [<Fact>]
    let ``should returns OK with empty list.`` () =
        Query.props { state with PropertySearch = Data.PropertySearch.NoSearch }
        |> shouldEqual (Ok [])

    [<Fact>]
    let ``should returns Error with 'Property not found'.`` () =
        Query.props { state with PropertySearch = Data.PropertySearch.Search "No" }
        |> shouldEqual (Error "Property not found")

    [<Fact>]
    let ``should returns Ok with non filtered properties.`` () =
        Query.props { state with PropertySearch = Data.PropertySearch.Search "" }
        |> shouldEqual (Ok ["Name"; "Attribute"; "Length"])

    [<Fact>]
    let ``should returns Ok with filtered properties.`` () =
        Query.props { state with PropertySearch = Data.PropertySearch.Search "Na" }
        |> shouldEqual (Ok [ "Name" ])

    [<Fact>]
    let ``should returns Ok with filtered properties when case sensitive.`` () =
        let state =
            caseSensitive { state with PropertySearch = Data.PropertySearch.Search "Na" }

        Query.props state
        |> shouldEqual (Ok [ "Name" ])

    [<Fact>]
    let ``should returns Ok with non filtered properties when rotate.`` () =
        Query.props { state with PropertySearch = Data.PropertySearch.Rotate ("", 0, ["Name"]) }
        |> shouldEqual (Ok ["Name"; "Attribute"; "Length"])

    [<Fact>]
    let ``should returns Ok with filtered properties when rotate.`` () =
        Query.props { state with PropertySearch = Data.PropertySearch.Rotate ("Na", 0, ["Name"]) }
        |> shouldEqual (Ok [ "Name" ])

module run =
    let duplicateCase (s: string) = [ s; s.ToLower() ]
    let mapToObj = List.map (PSObject.AsPSObject >> Data.Entry.Obj)

    let genList s =
        List.map duplicateCase s
        |> List.concat
        |> mapToObj

    let matcher m (s: Data.InternalState) = { s with QueryCondition.Matcher = m }

    let query q (s: Data.InternalState) = { s with QueryState.Query = q }

    let invert (s: Data.InternalState) = { s with QueryCondition.Invert = true }

    let opAnd (s: Data.InternalState) =
        { s with QueryCondition.Operator = Data.Operator.And }

    module ``with a simple query`` =
        let entries =
            genList [ "Name"
                      "Attribute"
                      "Length" ]

        let props = Map [ ("length", "Length") ]

        [<Fact>]
        let ``should returns empty if entry list is empty.`` () =
            let _, context = Query.prepare state
            Query.run context [] props
            |> shouldEqual []

        module ``of MATCH`` =
            let state = state |> matcher Data.Matcher.Match |> query "a"

            [<Fact>]
            let ``should returns all entries if query is empty.`` () =
                let state = initState () |> matcher Data.Matcher.Match
                let _, context = Query.prepare state

                Query.run context entries props
                |> shouldEqual entries

            [<Fact>]
            let ``should returns all entries if query is invalid pattern.`` () =
                let state = state |> query "+"
                let _, context = Query.prepare state
                Query.run context entries props
                |> shouldEqual entries

            [<Fact>]
            let ``should returns filtered entries.`` () =
                let _, context = Query.prepare state
                Query.run context entries props
                |> shouldEqual ( genList [ "Name"; "Attribute" ])

            [<Fact>]
            let ``should returns filtered entries when matcher is match and case sensitive.`` () =
                let state = caseSensitive state
                let _, context = Query.prepare state

                Query.run context entries props
                |> shouldEqual ( mapToObj [ "Name"; "name"; "attribute" ])

            [<Fact>]
            let ``should returns filtered entries when matcher is match and invert result.`` () =
                let state = invert state
                let _, context = Query.prepare state

                Query.run context entries props
                |> shouldEqual ( genList [ "Length" ])

            [<Fact>]
            let ``should returns filtered entries when composite query with or operator.`` () =
                let state = state |> query "a N"
                let _, context = Query.prepare state

                Query.run context entries props
                |> shouldEqual entries

            [<Fact>]
            let ``should returns filtered entries when composite query with and operator.`` () =
                let state = state |> query "a N" |> opAnd
                let _, context = Query.prepare state

                Query.run context entries props
                |> shouldEqual ( genList [ "Name" ])

        module ``of LIKE`` =
            let state = state |> matcher Data.Matcher.Like |> query "a*"

            [<Fact>]
            let ``should returns all entries if query is empty.`` () =
                let state = initState () |> matcher Data.Matcher.Like
                let _, context = Query.prepare state

                Query.run context entries props
                |> shouldEqual entries

            [<Fact>]
            let ``should returns matched entries when matcher is like .`` () =
                let _, context = Query.prepare state

                Query.run context entries props
                |> shouldEqual ( genList [ "Attribute" ])

            [<Fact>]
            let ``should returns matched entries when matcher is like and case sensitive.`` () =
                let state = caseSensitive state
                let _, context = Query.prepare state


                Query.run context entries props
                |> shouldEqual ( mapToObj [ "attribute" ])

            [<Fact>]
            let ``should returns filtered entries when matcher is like and invert result.`` () =
                let state = invert state
                let _, context = Query.prepare state


                Query.run context entries props
                |> shouldEqual ( genList [ "Name"; "Length" ])

            [<Fact>]
            let ``should returns filtered entries when composite query with or operator.`` () =
                let state = state |> query "*e* N*"
                let _, context = Query.prepare state

                Query.run context entries props
                |> shouldEqual entries

            [<Fact>]
            let ``should returns filtered entries when composite query with and operator.`` () =
                let state = state |> query "*e* N*" |> opAnd
                let _, context = Query.prepare state

                Query.run context entries props
                |> shouldEqual ( genList [ "Name" ])

        module ``of EQ`` =
            let state = state |> matcher Data.Matcher.Eq |> query "Name"

            [<Fact>]
            let ``should returns all entries if query is empty.`` () =
                let state = initState () |> matcher Data.Matcher.Eq
                let _, context = Query.prepare state

                Query.run context entries props
                |> shouldEqual entries

            [<Fact>]
            let ``should returns matched entries when matcher is eq .`` () =
                let _, context = Query.prepare state

                Query.run context entries props
                |> shouldEqual ( genList [ "Name" ])

            [<Fact>]
            let ``should returns matched entries when matcher is eq and case sensitive.`` () =
                let state = caseSensitive state
                let _, context = Query.prepare state

                Query.run context entries props
                |> shouldEqual ( mapToObj [ "Name" ])

            [<Fact>]
            let ``should returns filtered entries when matcher is eq and invert result.`` () =
                let state = invert state
                let _, context = Query.prepare state

                Query.run context entries props
                |> shouldEqual ( genList [ "Attribute"; "Length" ])

            [<Fact>]
            let ``should returns filtered entries when composite query with or operator.`` () =
                let state = state |> query "Name Length"
                let _, context = Query.prepare state

                Query.run context entries props
                |> shouldEqual ( genList [ "Name"; "Length" ])

            [<Fact>]
            let ``should returns filtered entries when composite query with and operator.`` () =
                let state = state |> query "Name Length" |> opAnd
                let _, context = Query.prepare state

                Query.run context entries props
                |> shouldEqual []

    module ``with a Dictionary query`` =
        let props = Map []

        open System.Collections
        let mapToDict = List.map Data.Entry.Dict

        let entries =
            mapToDict [ DictionaryEntry("John", "Doe")
                        DictionaryEntry("Jane", "Doe")
                        DictionaryEntry("Ming", "Wu")
                        DictionaryEntry("Taro", "Nanashi") ]

        [<Fact>]
        let ``should returns filtered entries when composite query with or operator.`` () =
            let state = state |> query "e"
            let _, context = Query.prepare state

            Query.run context entries props
            |> shouldEqual (
                mapToDict [ DictionaryEntry("John", "Doe")
                            DictionaryEntry("Jane", "Doe") ]
            )

        [<Fact>]
        let ``should returns filtered entries when composite query with and operator.`` () =
            let state = state |> query "e" |> opAnd
            let _, context = Query.prepare state

            Query.run context entries props
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
            let _, context = Query.prepare state

            Query.run context entries props
            |> shouldEqual (mapToDict [ DictionaryEntry("Jane", "Doe") ])

    module ``with a Property query`` =
        let getPsObj (f: string, l: string) =
            let ret = PSObject()

            [ "Fn", f; "Ln", l ]
            |> List.iter (fun (k, v) -> ret.Properties.Add(PSNoteProperty(k, v)))

            ret

        let mapToPsObj = List.map (getPsObj >> Data.Entry.Obj)

        let entries =
            mapToPsObj [ ("John", "Doe")
                         ("Jane", "Doe")
                         ("Ming", "Wu")
                         ("Taro", "Nanashi") ]

        let props = Map [ "fn", "Fn"; "ln", "Ln" ]

        [<Fact>]
        let ``should returns filtered entries when composite query with or operator.`` () =
            let state = state |> query ":fn a :ln d"
            let _, context = Query.prepare state

            let filtered =
                [ entries.[0]
                  entries.[1]
                  entries.[3] ]

            Query.run context entries props
            |> shouldEqual filtered

        [<Fact>]
        let ``should returns filtered entries when composite query with and operator.`` () =
            let state = state |> query ":fn a :ln d" |> opAnd
            let _, context = Query.prepare state
            let filtered = [ entries.[1] ]

            Query.run context entries props
            |> shouldEqual filtered

        [<Fact>]
        let ``should returns all entries when property not exists.`` () =
            let state = state |> query ":f a"
            let _, context = Query.prepare state

            Query.run context entries props
            |> shouldEqual entries

        [<Fact>]
        let ``should returns all entries when incomplete composite query.`` () =
            let state = state |> query ":fn "
            let _, context = Query.prepare state

            Query.run context entries props
            |> shouldEqual entries

        [<Fact>]
        let ``should returns filtered entries when incomplete composite query.`` () =
            let state = state |> query "a :fn"
            let _, context = Query.prepare state
            let filtered = [ entries.[1]; entries.[3] ]

            Query.run context entries props
            |> shouldEqual filtered
