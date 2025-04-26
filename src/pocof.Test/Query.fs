module PocofTest.Query

open System.Management.Automation

open Xunit
open FsUnitTyped

open Pocof

let initState () : Data.InternalState =
    { QueryState =
        { Query = ""
          Cursor = 0
          WindowBeginningCursor = 0
          WindowWidth = 0
          InputMode = Data.InputMode.Input }
      QueryCondition =
        { Matcher = Data.Matcher.Match
          Operator = Data.Operator.Or
          CaseSensitive = false
          Invert = false }
      PropertySearch = Data.PropertySearch.NoSearch
      SuppressProperties = false
      Refresh = Data.Refresh.Required }

let state = initState ()
let properties = [ "Name"; "Attribute"; "Length" ]

let caseSensitive (s: Data.InternalState) =
    { s with
        QueryCondition.CaseSensitive = true }

module DictionaryEntry =
    open System.Collections

    [<Fact>]
    let ``indexed property should return key.`` () =
        let d = DictionaryEntry("Jane", "Doe")
        d["Key"] |> shouldEqual "Jane"

    [<Fact>]
    let ``indexed property should return value.`` () =
        let d = DictionaryEntry("Jane", "Doe")
        d["Value"] |> shouldEqual "Doe"

    [<Fact>]
    let ``indexed property should return None.`` () =
        let d = DictionaryEntry("Jane", "Doe")
        d["Ke"] |> shouldEqual null

module PSObject =

    [<Fact>]
    let ``?->indexed property should return 1.`` () =
        let o = "a" |> PSObject.AsPSObject
        o["Length"] |> shouldEqual 1

    [<Fact>]
    let ``?->indexed property should return None.`` () =
        let o = "a" |> PSObject.AsPSObject
        o["Lengt"] |> shouldEqual null

module props =
    [<Fact>]
    let ``should return OK with empty list.`` () =
        Query.props
            properties
            { state with
                PropertySearch = Data.PropertySearch.NoSearch }
        |> shouldEqual (Ok [])

    [<Fact>]
    let ``should return Error with 'Property not found'.`` () =
        Query.props
            properties
            { state with
                PropertySearch = Data.PropertySearch.Search "No" }
        |> shouldEqual (Error "Property not found")

    [<Fact>]
    let ``should return Ok with non filtered properties.`` () =
        Query.props
            properties
            { state with
                PropertySearch = Data.PropertySearch.Search "" }
        |> shouldEqual (Ok [ "Name"; "Attribute"; "Length" ])

    [<Fact>]
    let ``should return Ok with filtered properties.`` () =
        Query.props
            properties
            { state with
                PropertySearch = Data.PropertySearch.Search "Na" }
        |> shouldEqual (Ok [ "Name" ])

    [<Fact>]
    let ``should return Ok with properties filtered in a case-insensitive manner.`` () =
        Query.props
            properties
            { state with
                PropertySearch = Data.PropertySearch.Search "na" }
        |> shouldEqual (Ok [ "Name" ])

    [<Fact>]
    let ``should return Ok with non filtered properties when rotate.`` () =
        Query.props
            properties
            { state with
                PropertySearch = Data.PropertySearch.Rotate("", [ "Name" ]) }
        |> shouldEqual (Ok [ "Name"; "Attribute"; "Length" ])

    [<Fact>]
    let ``should return Ok with filtered properties when rotate.`` () =
        Query.props
            properties
            { state with
                PropertySearch = Data.PropertySearch.Rotate("Na", [ "Name" ]) }
        |> shouldEqual (Ok [ "Name" ])

module run =
    open System.Collections

    let duplicateCase (s: string) = [ s; s.ToLower() ]
    let mapToObj = List.map (PSObject.AsPSObject >> Data.Entry.Obj)

    let genList s =
        List.collect duplicateCase s |> mapToObj

    let matcher m (s: Data.InternalState) = { s with QueryCondition.Matcher = m }

    let query q (s: Data.InternalState) = { s with QueryState.Query = q }

    let invert (s: Data.InternalState) = { s with QueryCondition.Invert = true }

    let opAnd (s: Data.InternalState) =
        { s with
            QueryCondition.Operator = Data.Operator.And }

    module ``with a simple query`` =
        let entries = genList [ "Name"; "Attribute"; "Length" ] |> PSeq.ofSeq

        let props = Map [ ("length", "Length") ]

        [<Fact>]
        let ``should return empty if entry list is empty.`` () =
            let context, _ = Query.prepare state
            Query.run context PSeq.empty props |> List.ofSeq |> shouldEqual []

        module ``of MATCH`` =
            let state = state |> matcher Data.Matcher.Match |> query "a"

            [<Fact>]
            let ``should return all entries if query is empty.`` () =
                let state = initState () |> matcher Data.Matcher.Match
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (entries |> List.ofSeq)

            [<Fact>]
            let ``should return all entries if query is invalid pattern.`` () =
                let state = state |> query "+"
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (entries |> List.ofSeq)

            [<Fact>]
            let ``should return filtered entries.`` () =
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (genList [ "Name"; "Attribute" ])

            [<Fact>]
            let ``should return filtered entries when matcher is match and case sensitive.`` () =
                let state = caseSensitive state
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (mapToObj [ "Name"; "name"; "attribute" ])

            [<Fact>]
            let ``should return filtered entries when matcher is match and invert result.`` () =
                let state = invert state
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (genList [ "Length" ])

            [<Fact>]
            let ``should return filtered entries when composite query with or operator.`` () =
                let state = state |> query "a N"
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (entries |> List.ofSeq)

            [<Fact>]
            let ``should return filtered entries when composite query with and operator.`` () =
                let state = state |> query "a N" |> opAnd
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (genList [ "Name" ])

        module ``of LIKE`` =
            let state = state |> matcher Data.Matcher.Like |> query "a*"

            [<Fact>]
            let ``should return all entries if query is empty.`` () =
                let state = initState () |> matcher Data.Matcher.Like
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (entries |> List.ofSeq)

            [<Fact>]
            let ``should return matched entries when matcher is like .`` () =
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (genList [ "Attribute" ])

            [<Fact>]
            let ``should return matched entries when matcher is like and case sensitive.`` () =
                let state = caseSensitive state
                let context, _ = Query.prepare state


                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (mapToObj [ "attribute" ])

            [<Fact>]
            let ``should return filtered entries when matcher is like and invert result.`` () =
                let state = invert state
                let context, _ = Query.prepare state


                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (genList [ "Name"; "Length" ])

            [<Fact>]
            let ``should return filtered entries when composite query with or operator.`` () =
                let state = state |> query "*e* N*"
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (entries |> List.ofSeq)

            [<Fact>]
            let ``should return filtered entries when composite query with and operator.`` () =
                let state = state |> query "*e* N*" |> opAnd
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (genList [ "Name" ])

        module ``of EQ`` =
            let state = state |> matcher Data.Matcher.Eq |> query "Name"

            [<Fact>]
            let ``should return all entries if query is empty.`` () =
                let state = initState () |> matcher Data.Matcher.Eq
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (entries |> List.ofSeq)

            [<Fact>]
            let ``should return matched entries when matcher is eq .`` () =
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (genList [ "Name" ])

            [<Fact>]
            let ``should return matched entries when matcher is eq and case sensitive.`` () =
                let state = caseSensitive state
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (mapToObj [ "Name" ])

            [<Fact>]
            let ``should return filtered entries when matcher is eq and invert result.`` () =
                let state = invert state
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (genList [ "Attribute"; "Length" ])

            [<Fact>]
            let ``should return filtered entries when composite query with or operator.`` () =
                let state = state |> query "Name Length"
                let context, _ = Query.prepare state

                Query.run context entries props
                |> List.ofSeq
                |> shouldEqual (genList [ "Name"; "Length" ])

            [<Fact>]
            let ``should return filtered entries when composite query with and operator.`` () =
                let state = state |> query "Name Length" |> opAnd
                let context, _ = Query.prepare state

                Query.run context entries props |> List.ofSeq |> shouldEqual []

    module ``with a Dictionary query`` =
        let props = Map []
        let mapToDict = List.map Data.Entry.Dict

        let entries =
            mapToDict
                [ DictionaryEntry("John", "Doe")
                  DictionaryEntry("Jane", "Doe")
                  DictionaryEntry("Ming", "Wu")
                  DictionaryEntry("Taro", "Nanashi") ]
            |> PSeq.ofSeq

        [<Fact>]
        let ``should return filtered entries when composite query with or operator.`` () =
            let state = state |> query "e"
            let context, _ = Query.prepare state

            Query.run context entries props
            |> List.ofSeq
            |> shouldEqual (mapToDict [ DictionaryEntry("John", "Doe"); DictionaryEntry("Jane", "Doe") ])

        [<Fact>]
        let ``should return filtered entries when composite query with and operator.`` () =
            let state = state |> query "ne" |> opAnd
            let context, _ = Query.prepare state

            Query.run context entries props
            |> List.ofSeq
            |> shouldEqual (mapToDict [ DictionaryEntry("Jane", "Doe") ])

        [<Fact>]
        let ``should return filtered entries when property query.`` () =
            let props =
                DictionaryEntry("Jane", "Doe")
                |> PSObject.AsPSObject
                |> _.Properties
                |> Seq.map (fun p -> p.Name.ToLower(), p.Name)
                |> Map

            let state = state |> query ":key  ja" |> opAnd
            let context, _ = Query.prepare state

            Query.run context entries props
            |> List.ofSeq
            |> shouldEqual (mapToDict [ DictionaryEntry("Jane", "Doe") ])

        [<Fact>]
        let ``should return all entries when querying a non-existing property.`` () =
            let props =
                DictionaryEntry("Jane", "Doe")
                |> PSObject.AsPSObject
                |> _.Properties
                |> Seq.map (fun p -> p.Name.ToLower(), p.Name)
                |> Map

            let state = state |> query ":title  ja" |> opAnd
            let context, _ = Query.prepare state

            Query.run context entries props
            |> List.ofSeq
            |> shouldEqual (entries |> List.ofSeq)

        [<Fact>]
        let ``should return empty when querying a non-existing property.`` () =
            let props =
                DictionaryEntry("Jane", "Doe")
                |> PSObject.AsPSObject
                |> _.Properties
                |> Seq.map (fun p -> p.Name.ToLower(), p.Name)
                |> Map

            let state = state |> query ":key ja" |> opAnd

            let entries =
                [ "d" ] |> List.map (PSObject.AsPSObject >> Data.Entry.Obj) |> PSeq.ofSeq

            let context, _ = Query.prepare state

            Query.run context entries props |> List.ofSeq |> shouldEqual (List.empty)

    module ``with a Property query`` =
        let getPsObj (f: string, l: string) =
            let ret = PSObject()

            [ "Fn", f; "Ln", l ]
            |> List.iter (fun (k, v) -> ret.Properties.Add(PSNoteProperty(k, v)))

            ret

        let mapToPsObj = List.map (getPsObj >> Data.Entry.Obj)

        let entries =
            mapToPsObj [ ("John", "Doe"); ("Jane", "Doe"); ("Ming", "Wu"); ("Taro", "Nanashi") ]

        let props = Map [ "fn", "Fn"; "ln", "Ln" ]

        [<Fact>]
        let ``should return filtered entries when composite query with or operator.`` () =
            let state = state |> query ":fn a  :ln  d"
            let context, _ = Query.prepare state

            let filtered = [ entries.[0]; entries.[1]; entries.[3] ]

            Query.run context (entries |> PSeq.ofSeq) props
            |> List.ofSeq
            |> shouldEqual filtered

        [<Fact>]
        let ``should return filtered entries when composite query with and operator.`` () =
            let state = state |> query ":fn a :ln d" |> opAnd
            let context, _ = Query.prepare state
            let filtered = [ entries.[1] ]

            Query.run context (entries |> PSeq.ofSeq) props
            |> List.ofSeq
            |> shouldEqual filtered

        [<Fact>]
        let ``should return all entries when property not exists.`` () =
            let state = state |> query ":f a"
            let context, _ = Query.prepare state

            Query.run context (entries |> PSeq.ofSeq) props
            |> List.ofSeq
            |> shouldEqual entries


        [<Fact>]
        let ``should return all entries when incomplete composite query.`` () =
            let state = state |> query ":fn "
            let context, _ = Query.prepare state

            Query.run context (entries |> PSeq.ofSeq) props
            |> List.ofSeq
            |> shouldEqual entries

        [<Fact>]
        let ``should return filtered entries when incomplete composite query.`` () =
            let state = state |> query "a :fn "
            let context, _ = Query.prepare state
            let filtered = [ entries.[1]; entries.[3] ]

            Query.run context (entries |> PSeq.ofSeq) props
            |> List.ofSeq
            |> shouldEqual filtered

        [<Fact>]
        let ``should return filtered entries when a non-existent property query exists after a correct query`` () =
            let state = state |> query "a :f  e "
            let context, _ = Query.prepare state
            let filtered = [ entries.[1]; entries.[3] ]

            Query.run context (entries |> PSeq.ofSeq) props
            |> List.ofSeq
            |> shouldEqual filtered

        [<Fact>]
        let ``should return filtered entries when a non-existent property query exists before a correct query`` () =
            let state = state |> query ":f e a"
            let context, _ = Query.prepare state
            let filtered = [ entries.[1]; entries.[3] ]

            Query.run context (entries |> PSeq.ofSeq) props
            |> List.ofSeq
            |> shouldEqual filtered

    module ``with a locale`` =
        open System.Globalization

        let entries =
            [ System.DateTime.Parse("2024-01-01")
              System.DateTime.Parse("2024-01-02")
              System.DateTime.Parse("2024-01-03")
              System.DateTime.Parse("2024-01-04") ]
            |> List.map (PSObject.AsPSObject >> Data.Entry.Obj)

        [<Fact>]
        let ``should return filtered entries when composite query with or operator.`` () =
            let culture = System.Threading.Thread.CurrentThread.CurrentCulture
            let testCulture = CultureInfo.GetCultureInfo("en-US").Clone() :?> CultureInfo
            testCulture.DateTimeFormat.ShortDatePattern <- "yyyy-MM-dd"
            System.Threading.Thread.CurrentThread.CurrentCulture <- testCulture

            let state = state |> query "01-04"
            let context, _ = Query.prepare state

            let filtered = [ entries |> List.last ]

            Query.run context (entries |> PSeq.ofSeq) (Map [])
            |> List.ofSeq
            |> shouldEqual filtered

            System.Threading.Thread.CurrentThread.CurrentCulture <- culture
