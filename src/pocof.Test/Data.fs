module PocofTest.Data

open System
open Microsoft.FSharp.Reflection

open Xunit
open FsUnitTyped

open Pocof
open Pocof.Data

module unwrap =
    open System.Collections
    open System.Management.Automation

    [<Fact>]
    let ``should returns "a".`` () =
        unwrap [ Entry.Obj(PSObject.AsPSObject "a") ]
        |> shouldEqual [ PSObject.AsPSObject "a" ]

    [<Fact>]
    let ``should returns dictionary.`` () =
        unwrap [ Entry.Dict(DictionaryEntry("Jane", "Doe")) ]
        |> shouldEqual [ DictionaryEntry("Jane", "Doe") ]

module ``Action fromString should returns`` =
    [<Fact>]
    let ``Error Unknown.`` () =
        Action.fromString "XXX" |> shouldEqual (Error "Unknown Action 'XXX'.")

    [<Fact>]
    let ``Error when AddQuery.`` () =
        Action.fromString "AddQuery" |> shouldEqual (Error "Unknown Action 'AddQuery'.")

    [<Fact>]
    let ``known actions excluding AddQuery.`` () =
        FSharpType.GetUnionCases(typeof<Action>)
        |> Seq.filter (fun a -> a.Name <> "AddQuery")
        |> Seq.iter (fun a ->
            [ a.Name; String.lower a.Name; String.upper a.Name ]
            |> List.map Action.fromString
            |> List.iter (shouldEqual (Ok(FSharpValue.MakeUnion(a, [||]) :?> Action))))

let ``Error Unknown.``<'a> (fromString: string -> 'a) =
    shouldFail (fun () -> fromString "Unknown" |> ignore)

let ``known matchers.``<'a> (fromString: string -> 'a) =
    FSharpType.GetUnionCases(typeof<'a>)
    |> Seq.iter (fun (a: UnionCaseInfo) ->
        [ a.Name; String.lower a.Name; String.upper a.Name ]
        |> List.map fromString
        |> List.iter (shouldEqual (FSharpValue.MakeUnion(a, [||]) :?> 'a)))

module ``Matcher fromString should returns`` =
    [<Fact>]
    let ``Error Unknown.`` () =
        ``Error Unknown.``<Matcher> Matcher.fromString

    [<Fact>]
    let ``known matchers.`` () =
        ``known matchers.``<Matcher> Matcher.fromString

module ``Operator fromString should returns`` =
    [<Fact>]
    let ``Error Unknown.`` () =
        ``Error Unknown.``<Operator> Operator.fromString

    [<Fact>]
    let ``known matchers.`` () =
        ``known matchers.``<Operator> Operator.fromString

module ``Layout fromString should returns`` =
    [<Fact>]
    let ``Error Unknown.`` () =
        ``Error Unknown.``<Layout> Layout.fromString

    [<Fact>]
    let ``known matchers.`` () =
        ``known matchers.``<Layout> Layout.fromString

module ``QueryState toString should returns`` =
    let queryState (m: Matcher) (o: Operator) : QueryCondition =
        { Matcher = m
          Operator = o
          CaseSensitive = false
          Invert = false }

    let caseSensitive (s: QueryCondition) = { s with CaseSensitive = true }
    let invert (s: QueryCondition) = { s with Invert = true }

    [<Fact>]
    let ``eq and`` () =
        let actual = queryState Matcher.Eq Operator.And
        string actual |> shouldEqual "eq and"

    [<Fact>]
    let ``cne or`` () =
        let actual = queryState Matcher.Eq Operator.Or |> caseSensitive |> invert

        string actual |> shouldEqual "cne or"

    [<Fact>]
    let ``ceq and`` () =
        let actual = queryState Matcher.Eq Operator.And |> caseSensitive

        string actual |> shouldEqual "ceq and"

    [<Fact>]
    let ``ne or`` () =
        let actual = queryState Matcher.Eq Operator.Or |> invert
        string actual |> shouldEqual "ne or"

    [<Fact>]
    let ``like and`` () =
        let actual = queryState Matcher.Like Operator.And
        string actual |> shouldEqual "like and"

    [<Fact>]
    let ``clike and`` () =
        let actual = queryState Matcher.Like Operator.And |> caseSensitive

        string actual |> shouldEqual "clike and"

    [<Fact>]
    let ``notlike and`` () =
        let actual = queryState Matcher.Like Operator.And |> invert
        string actual |> shouldEqual "notlike and"

    [<Fact>]
    let ``cnotlike and`` () =
        let actual = queryState Matcher.Like Operator.And |> caseSensitive |> invert

        string actual |> shouldEqual "cnotlike and"

    [<Fact>]
    let ``cnotmatch or`` () =
        let actual = queryState Matcher.Match Operator.Or |> caseSensitive |> invert

        string actual |> shouldEqual "cnotmatch or"

    [<Fact>]
    let ``notmatch or`` () =
        let actual = queryState Matcher.Match Operator.Or |> invert
        string actual |> shouldEqual "notmatch or"

    [<Fact>]
    let ``cmatch or`` () =
        let actual = queryState Matcher.Match Operator.Or |> caseSensitive

        string actual |> shouldEqual "cmatch or"

    [<Fact>]
    let ``match or`` () =
        let actual = queryState Matcher.Match Operator.Or
        string actual |> shouldEqual "match or"

module initConfig =
    [<Fact>]
    let ``should returns tuples`` () =
        initConfig
            { Query = ":name"
              Matcher = "like"
              Operator = "and"
              CaseSensitive = true
              InvertQuery = true
              NotInteractive = true
              SuppressProperties = true
              Prompt = "prompt"
              Layout = "TopDown"
              Keymaps = Map [ ({ Modifier = 7; Key = ConsoleKey.X }, Action.Cancel) ]
              Properties = [ "name"; "attributes" ]
              EntryCount = 10
              ConsoleWidth = 60
              ConsoleHeight = 20 }
        |> shouldEqual (
            { Layout = Layout.TopDown
              Keymaps = Map [ ({ Modifier = 7; Key = ConsoleKey.X }, Action.Cancel) ]
              NotInteractive = true },
            { QueryState =
                { Query = ":name"
                  Cursor = 5
                  WindowBeginningCursor = 0
                  WindowWidth = 60 - (String.length "prompt>") - (String.length " cnotlike and [10]")
                  InputMode = InputMode.Input }
              QueryCondition =
                { Matcher = Matcher.Like
                  Operator = Operator.And
                  CaseSensitive = true
                  Invert = true }
              PropertySearch = PropertySearch.Search "name"
              Notification = ""
              SuppressProperties = true
              Properties = [ "name"; "attributes" ]
              Prompt = "prompt"
              FilteredCount = 10
              ConsoleWidth = 60
              Refresh = Refresh.Required },
            { Y = 0; Height = 20 }
        )

    [<Fact>]
    let ``should fail due to unknown Matcher.`` () =
        shouldFail (fun () ->
            initConfig
                { Query = ""
                  Matcher = "same"
                  Operator = "or"
                  CaseSensitive = false
                  InvertQuery = false
                  NotInteractive = false
                  SuppressProperties = false
                  Prompt = "prompt"
                  Layout = "TopDown"
                  Keymaps = Map []
                  Properties = []
                  EntryCount = 10
                  ConsoleWidth = 20
                  ConsoleHeight = 20 }
            |> ignore)

    [<Fact>]
    let ``should fail due to unknown Operator.`` () =
        shouldFail (fun () ->
            initConfig
                { Query = ""
                  Matcher = "eq"
                  Operator = "not"
                  CaseSensitive = false
                  InvertQuery = false
                  NotInteractive = false
                  SuppressProperties = false
                  Prompt = "prompt"
                  Layout = "TopDown"
                  Keymaps = Map []
                  Properties = []
                  EntryCount = 10
                  ConsoleWidth = 20
                  ConsoleHeight = 20 }

            |> ignore)

    [<Fact>]
    let ``should fail due to unknown Layout.`` () =
        shouldFail (fun () ->
            initConfig
                { Query = ""
                  Matcher = "eq"
                  Operator = "or"
                  CaseSensitive = false
                  InvertQuery = false
                  NotInteractive = false
                  SuppressProperties = false
                  Prompt = "prompt"
                  Layout = "LeftToRight"
                  Keymaps = Map []
                  Properties = []
                  EntryCount = 10
                  ConsoleWidth = 20
                  ConsoleHeight = 20 }
            |> ignore)

module QueryState =
    module getCurrentProperty =
        let qs q x =
            { Query = q
              Cursor = x
              WindowBeginningCursor = 0
              WindowWidth = 0
              InputMode = InputMode.Input }

        [<Fact>]
        let ``should returns NoSearch when no colon`` () =
            QueryState.getCurrentProperty (qs "a" 1) |> shouldEqual PropertySearch.NoSearch

        [<Fact>]
        let ``should returns Search with "a" when start with colon`` () =
            QueryState.getCurrentProperty (qs ":a" 2)
            |> shouldEqual (PropertySearch.Search "a")

        [<Fact>]
        let ``should returns Search with "a" when start with colon and cursor position 1`` () =
            QueryState.getCurrentProperty (qs ":a" 1)
            |> shouldEqual (PropertySearch.Search "")

        [<Fact>]
        let ``should returns Search with "a" when start with colon and trailing space`` () =
            QueryState.getCurrentProperty (qs ":a " 2)
            |> shouldEqual (PropertySearch.Search "a")

        [<Fact>]
        let ``should returns NoSearch when start with colon and cursor position 3`` () =
            QueryState.getCurrentProperty (qs ":a " 3)
            |> shouldEqual (PropertySearch.NoSearch)

        [<Fact>]
        let ``should returns Search with "a" when start with colon and trailing keyword `` () =
            QueryState.getCurrentProperty (qs ":a a" 2)
            |> shouldEqual (PropertySearch.Search "a")

    module deleteSelection =
        [<Fact>]
        let ``should returns no change when InputMode.Input `` () =
            let state =
                { Query = ""
                  Cursor = 0
                  WindowBeginningCursor = 0
                  WindowWidth = 0
                  InputMode = InputMode.Input }

            QueryState.deleteSelection state |> shouldEqual state
