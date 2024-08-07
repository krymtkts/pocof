module PocofTest.Data

open System
open Microsoft.FSharp.Reflection

open Xunit
open FsUnitTyped

open Pocof
open Pocof.Data

module LanguageExtension =
    module Option =
        type Mock() =
            member val disposed = false with get, set

            interface IDisposable with
                member __.Dispose() = __.disposed <- true

        [<Fact>]
        let ``shouldn't call Dispose if Some.`` () =
            let mock = new Mock()
            mock |> Some |> Option.dispose
            mock.disposed |> shouldEqual true
            Option.dispose <| Some mock

        [<Fact>]
        let ``shouldn't call Dispose if None.`` () =
            // NOTE: only for coverage.
            None |> Option.dispose

module unwrap =
    open System.Collections
    open System.Management.Automation

    [<Fact>]
    let ``should return "a".`` () =
        unwrap [ Entry.Obj(PSObject.AsPSObject "a") ]
        |> List.ofSeq
        |> shouldEqual [ PSObject.AsPSObject "a" ]

    [<Fact>]
    let ``should return dictionary.`` () =
        unwrap [ Entry.Dict(DictionaryEntry("Jane", "Doe")) ]
        |> List.ofSeq
        |> shouldEqual [ DictionaryEntry("Jane", "Doe") ]

module ``Action fromString`` =
    [<Fact>]
    let ``should return Error Unknown.`` () =
        Action.fromString "XXX" |> shouldEqual (Error "Unknown Action 'XXX'.")

    [<Fact>]
    let ``should return Error when AddQuery.`` () =
        Action.fromString "AddQuery" |> shouldEqual (Error "Unknown Action 'AddQuery'.")

    [<Fact>]
    let ``should return known actions excluding AddQuery.`` () =
        FSharpType.GetUnionCases(typeof<Action>)
        |> Seq.filter (fun a -> a.Name <> "AddQuery")
        |> Seq.iter (fun a ->
            [ a.Name; String.lower a.Name; String.upper a.Name ]
            |> List.map Action.fromString
            |> List.iter (shouldEqual (Ok(FSharpValue.MakeUnion(a, [||]) :?> Action))))

module fromString =
    let ``Error Unknown.``<'a> (fromString: string -> 'a) =
        shouldFail (fun () -> fromString "Unknown" |> ignore)

    let ``known matchers.``<'a> (fromString: string -> 'a) =
        FSharpType.GetUnionCases(typeof<'a>)
        |> Seq.iter (fun (a: UnionCaseInfo) ->
            [ a.Name; String.lower a.Name; String.upper a.Name ]
            |> List.map fromString
            |> List.iter (shouldEqual (FSharpValue.MakeUnion(a, [||]) :?> 'a)))

    module ``of Matcher`` =
        [<Fact>]
        let ``should return Error Unknown.`` () =
            ``Error Unknown.``<Matcher> Matcher.fromString

        [<Fact>]
        let ``should return known matchers.`` () =
            ``known matchers.``<Matcher> Matcher.fromString

    module ``of Operator`` =
        [<Fact>]
        let ``should return Error Unknown.`` () =
            ``Error Unknown.``<Operator> Operator.fromString

        [<Fact>]
        let ``should return known matchers.`` () =
            ``known matchers.``<Operator> Operator.fromString

    module ``of Layout`` =
        [<Fact>]
        let ``should return Error Unknown.`` () =
            ``Error Unknown.``<Layout> Layout.fromString

        [<Fact>]
        let ``should return known matchers.`` () =
            ``known matchers.``<Layout> Layout.fromString

module ``QueryState toString`` =
    let queryState (m: Matcher) (o: Operator) : QueryCondition =
        { Matcher = m
          Operator = o
          CaseSensitive = false
          Invert = false }

    let caseSensitive (s: QueryCondition) = { s with CaseSensitive = true }
    let invert (s: QueryCondition) = { s with Invert = true }

    [<Fact>]
    let ``should return eq and`` () =
        let actual = queryState Matcher.Eq Operator.And
        string actual |> shouldEqual "eq and"

    [<Fact>]
    let ``should return cne or`` () =
        let actual = queryState Matcher.Eq Operator.Or |> caseSensitive |> invert

        string actual |> shouldEqual "cne or"

    [<Fact>]
    let ``should return ceq and`` () =
        let actual = queryState Matcher.Eq Operator.And |> caseSensitive

        string actual |> shouldEqual "ceq and"

    [<Fact>]
    let ``should return ne or`` () =
        let actual = queryState Matcher.Eq Operator.Or |> invert
        string actual |> shouldEqual "ne or"

    [<Fact>]
    let ``should return like and`` () =
        let actual = queryState Matcher.Like Operator.And
        string actual |> shouldEqual "like and"

    [<Fact>]
    let ``should return clike and`` () =
        let actual = queryState Matcher.Like Operator.And |> caseSensitive

        string actual |> shouldEqual "clike and"

    [<Fact>]
    let ``should return notlike and`` () =
        let actual = queryState Matcher.Like Operator.And |> invert
        string actual |> shouldEqual "notlike and"

    [<Fact>]
    let ``should return cnotlike and`` () =
        let actual = queryState Matcher.Like Operator.And |> caseSensitive |> invert

        string actual |> shouldEqual "cnotlike and"

    [<Fact>]
    let ``should return cnotmatch or`` () =
        let actual = queryState Matcher.Match Operator.Or |> caseSensitive |> invert

        string actual |> shouldEqual "cnotmatch or"

    [<Fact>]
    let ``should return notmatch or`` () =
        let actual = queryState Matcher.Match Operator.Or |> invert
        string actual |> shouldEqual "notmatch or"

    [<Fact>]
    let ``should return cmatch or`` () =
        let actual = queryState Matcher.Match Operator.Or |> caseSensitive

        string actual |> shouldEqual "cmatch or"

    [<Fact>]
    let ``should return match or`` () =
        let actual = queryState Matcher.Match Operator.Or
        string actual |> shouldEqual "match or"

module initConfig =
    [<Fact>]
    let ``should return tuples`` () =
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
              PropertiesMap = Map [ ("name", "name"); ("attributes", "attributes") ]
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
              PropertyMap = Map [ ("name", "name"); ("attributes", "attributes") ]
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
                  PropertiesMap = Map []
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
                  PropertiesMap = Map []
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
                  PropertiesMap = Map []
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
        let ``should return NoSearch when no colon`` () =
            QueryState.getCurrentProperty (qs "a" 1) |> shouldEqual PropertySearch.NoSearch

        [<Fact>]
        let ``should return Search with "a" when start with colon`` () =
            QueryState.getCurrentProperty (qs ":a" 2)
            |> shouldEqual (PropertySearch.Search "a")

        [<Fact>]
        let ``should return Search with "a" when start with colon and cursor position 1`` () =
            QueryState.getCurrentProperty (qs ":a" 1)
            |> shouldEqual (PropertySearch.Search "")

        [<Fact>]
        let ``should return Search with "a" when start with colon and trailing space`` () =
            QueryState.getCurrentProperty (qs ":a " 2)
            |> shouldEqual (PropertySearch.Search "a")

        [<Fact>]
        let ``should return NoSearch when start with colon and cursor position 3`` () =
            QueryState.getCurrentProperty (qs ":a " 3)
            |> shouldEqual (PropertySearch.NoSearch)

        [<Fact>]
        let ``should return Search with "a" when start with colon and trailing keyword `` () =
            QueryState.getCurrentProperty (qs ":a a" 2)
            |> shouldEqual (PropertySearch.Search "a")

    module deleteSelection =
        [<Fact>]
        let ``should return no change when InputMode.Input `` () =
            let state =
                { Query = ""
                  Cursor = 0
                  WindowBeginningCursor = 0
                  WindowWidth = 0
                  InputMode = InputMode.Input }

            QueryState.deleteSelection state |> shouldEqual state
