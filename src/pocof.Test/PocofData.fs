module PocofData

open Xunit
open FsUnitTyped
open Microsoft.FSharp.Reflection
open System
open pocof
open PocofData

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
    let queryState (m: Matcher) (o: Operator) : QueryState =
        { Matcher = m
          Operator = o
          CaseSensitive = false
          Invert = false }

    let caseSensitive (s: QueryState) = { s with CaseSensitive = true }
    let invert (s: QueryState) = { s with Invert = true }

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
              Properties = [ "name"; "attributes" ] }
        |> shouldEqual (
            { Prompt = "prompt"
              Layout = Layout.TopDown
              Keymaps = Map [ ({ Modifier = 7; Key = ConsoleKey.X }, Cancel) ]
              NotInteractive = true },
            { Query = ":name"
              QueryState =
                { Matcher = LIKE
                  Operator = AND
                  CaseSensitive = true
                  Invert = true }
              PropertySearch = Search "name"
              Notification = ""
              SuppressProperties = true
              Properties = [ "name"; "attributes" ]
              Refresh = Required },
            { X = 5; Y = 0 }
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
                  Properties = [] }
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
                  Properties = [] }
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
                  Properties = [] }
            |> ignore)
