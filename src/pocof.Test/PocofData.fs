module PocofData

open Xunit
open FsUnitTyped
open Microsoft.FSharp.Reflection
open System
open pocof
open PocofData

module unwrap =
    open System.Management.Automation
    open System.Collections

    [<Fact>]
    let ``should returns "a".`` () =
        unwrap [ Obj(PSObject.AsPSObject "a") ]
        |> shouldEqual [ PSObject.AsPSObject "a" ]

    [<Fact>]
    let ``should returns dictionary.`` () =
        unwrap [ Dict(DictionaryEntry("Jane", "Doe")) ]
        |> shouldEqual [ DictionaryEntry("Jane", "Doe") ]

module ``Action fromString should returns`` =
    [<Fact>]
    let ``Error Unknown.`` () =
        Action.fromString "XXX"
        |> shouldEqual (Error "Unknown Action 'XXX'.")

    [<Fact>]
    let ``Error when AddQuery.`` () =
        Action.fromString "AddQuery"
        |> shouldEqual (Error "Unknown Action 'AddQuery'.")

    [<Fact>]
    let ``known actions excluding AddQuery.`` () =
        FSharpType.GetUnionCases(typeof<Action>)
        |> Seq.filter (fun a -> a.Name <> "AddQuery")
        |> Seq.iter (fun a ->
            [ a.Name
              String.lower a.Name
              String.upper a.Name ]
            |> List.map Action.fromString
            |> List.iter (shouldEqual (Ok(FSharpValue.MakeUnion(a, [||]) :?> Action))))

let ``Error Unknown.``<'a> (fromString: string -> 'a) =
    shouldFail (fun () -> fromString "Unknown" |> ignore)

let ``known matchers.``<'a> (fromString: string -> 'a) =
    FSharpType.GetUnionCases(typeof<'a>)
    |> Seq.iter (fun (a: UnionCaseInfo) ->
        [ a.Name
          String.lower a.Name
          String.upper a.Name ]
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
        let actual = queryState EQ AND
        string actual |> shouldEqual "eq and"

    [<Fact>]
    let ``cne or`` () =
        let actual = queryState EQ OR |> caseSensitive |> invert
        string actual |> shouldEqual "cne or"

    [<Fact>]
    let ``ceq and`` () =
        let actual = queryState EQ AND |> caseSensitive
        string actual |> shouldEqual "ceq and"

    [<Fact>]
    let ``ne or`` () =
        let actual = queryState EQ OR |> invert
        string actual |> shouldEqual "ne or"

    [<Fact>]
    let ``like and`` () =
        let actual = queryState LIKE AND
        string actual |> shouldEqual "like and"

    [<Fact>]
    let ``clike and`` () =
        let actual = queryState LIKE AND |> caseSensitive
        string actual |> shouldEqual "clike and"

    [<Fact>]
    let ``notlike and`` () =
        let actual = queryState LIKE AND |> invert
        string actual |> shouldEqual "notlike and"

    [<Fact>]
    let ``notclike and`` () =
        let actual = queryState LIKE AND |> caseSensitive |> invert
        string actual |> shouldEqual "notclike and"

    [<Fact>]
    let ``notcmatch or`` () =
        let actual = queryState MATCH OR |> caseSensitive |> invert
        string actual |> shouldEqual "notcmatch or"

    [<Fact>]
    let ``notmatch or`` () =
        let actual = queryState MATCH OR |> invert
        string actual |> shouldEqual "notmatch or"

    [<Fact>]
    let ``cmatch or`` () =
        let actual = queryState MATCH OR |> caseSensitive
        string actual |> shouldEqual "cmatch or"

    [<Fact>]
    let ``match or`` () =
        let actual = queryState MATCH OR
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
              Keymaps = Map [ ({ Modifier = 7; Key = ConsoleKey.X }, Cancel) ]
              Properties = [ "name"; "attributes" ]
              EntryCount = 10
              ConsoleWidth = 60
              ConsoleHeight = 20 }
        |> shouldEqual (
            { Layout = Layout.TopDown
              Keymaps = Map [ ({ Modifier = 7; Key = ConsoleKey.X }, Cancel) ]
              NotInteractive = true },
            { QueryState =
                { Query = ":name"
                  Cursor = 5
                  WindowBeginningX = 0
                  WindowWidth =
                    60
                    - (String.length "prompt>")
                    - (String.length " notclike and [10]") }
              QueryCondition =
                { Matcher = LIKE
                  Operator = AND
                  CaseSensitive = true
                  Invert = true }
              PropertySearch = Search "name"
              Notification = ""
              SuppressProperties = true
              Properties = [ "name"; "attributes" ]
              Prompt = "prompt"
              FilteredCount = 10
              ConsoleWidth = 60
              Refresh = Required },
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
              WindowBeginningX = 0
              WindowWidth = 0 }

        [<Fact>]
        let ``should returns NoSearch when no colon`` () =
            QueryState.getCurrentProperty (qs "a" 1)
            |> shouldEqual NoSearch

        [<Fact>]
        let ``should returns Search with "a" when start with colon`` () =
            QueryState.getCurrentProperty (qs ":a" 2)
            |> shouldEqual (Search "a")

        [<Fact>]
        let ``should returns Search with "a" when start with colon and cursor position 1`` () =
            QueryState.getCurrentProperty (qs ":a" 1)
            |> shouldEqual (Search "")

        [<Fact>]
        let ``should returns Search with "a" when start with colon and trailing space`` () =
            QueryState.getCurrentProperty (qs ":a " 2)
            |> shouldEqual (Search "a")

        [<Fact>]
        let ``should returns NoSearch when start with colon and cursor position 3`` () =
            QueryState.getCurrentProperty (qs ":a " 3)
            |> shouldEqual (NoSearch)

        [<Fact>]
        let ``should returns Search with "a" when start with colon and trailing keyword `` () =
            QueryState.getCurrentProperty (qs ":a a" 2)
            |> shouldEqual (Search "a")
