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
        |> shouldEqual (Error "Unknown case 'XXX'.")

    [<Fact>]
    let ``Error when AddQuery.`` () =
        Action.fromString "AddQuery"
        |> shouldEqual (Error "Unknown case 'AddQuery'.")

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
              Keymaps = Map [ ({ Modifier = 7; Key = ConsoleKey.X }, Cancel) ] }
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
              SuppressProperties = true },
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
                  Keymaps = Map [] }
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
                  Keymaps = Map [] }
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
                  Keymaps = Map [] }
            |> ignore)

module invokeAction =
    let state: InternalState =
        { Query = ""
          QueryState =
            { Matcher = MATCH
              Operator = OR
              CaseSensitive = false
              Invert = false }
          PropertySearch = NoSearch
          Notification = ""
          SuppressProperties = false }

    let position: Position = { X = 0; Y = 0 }

    module ``with AddQuery`` =
        [<Fact>]
        let ``should return a property search state and position.x = 1 when the char is colon.`` () =
            invokeAction state { X = 0; Y = 0 } [] (AddQuery ":")
            |> shouldEqual (
                { state with
                    Query = ":"
                    PropertySearch = Search "" },
                { X = 1; Y = 0 },
                Required
            )

        [<Fact>]
        let ``should return a non-search state and position.X = 6 when the char is space.`` () =
            invokeAction
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }
                { X = 5; Y = 0 }
                []
                (AddQuery " ")
            |> shouldEqual (
                { state with
                    Query = ":name "
                    PropertySearch = NoSearch },
                { X = 6; Y = 0 },
                Required
            )

    module ``with BackwardChar`` =
        [<Fact>]
        let ``should return state with pos unmodified when moving forward on ':name' with position.X=0.`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = NoSearch }

            let position: Position = { X = 0; Y = 0 }

            invokeAction state position [] BackwardChar
            |> shouldEqual (state, position, NotRequired)

        [<Fact>]
        let ``should return state with position.X=4 when moving forward on ':name' with position.X=5.`` () =
            invokeAction
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }
                { X = 5; Y = 0 }
                []
                BackwardChar
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Search "nam" },
                { X = 4; Y = 0 },
                Required
            )

    module ``with ForwardChar`` =
        [<Fact>]
        let ``should return state with position.X=2 when moving forward on ':name' with position.X=1.`` () =
            invokeAction
                { state with
                    Query = ":name"
                    PropertySearch = Search "" }
                { X = 1; Y = 0 }
                []
                ForwardChar
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Search "n" },
                { X = 2; Y = 0 },
                Required
            )

        [<Fact>]
        let ``should return state with pos unmodified when moving forward on ':name' with position.X=5 and query.Length=3.``
            ()
            =
            invokeAction
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }
                { X = 5; Y = 0 }
                []
                ForwardChar
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" },
                { X = 5; Y = 0 },
                NotRequired
            )

    module ``with BeginningOfLine`` =
        [<Fact>]
        let ``should return state with position.X = 0.`` () =
            invokeAction
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }
                { X = 5; Y = 0 }
                []
                BeginningOfLine
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = NoSearch },
                { X = 0; Y = 0 },
                Required
            )

        [<Fact>]
        let ``should return state with pos unmodified`` () =
            invokeAction
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }
                { X = 0; Y = 0 }
                []
                BeginningOfLine
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = NoSearch },
                { X = 0; Y = 0 },
                NotRequired
            )

        [<Fact>]
        let ``should return no change with pos modified when NonSearch`` () =
            invokeAction
                { state with
                    Query = "query"
                    PropertySearch = NoSearch }
                { X = 5; Y = 0 }
                []
                BeginningOfLine
            |> (shouldEqual (
                { state with
                    Query = "query"
                    PropertySearch = NoSearch },
                { X = 0; Y = 0 },
                NotRequired
            ))

    module ``with EndOfLine`` =
        [<Fact>]
        let ``should return state with position.X = query length.`` () =
            invokeAction
                { state with
                    Query = ":name"
                    PropertySearch = Search "n" }
                { X = 2; Y = 0 }
                []
                EndOfLine
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" },
                { X = 5; Y = 0 },
                Required
            )

        [<Fact>]
        let ``should return state with pos unmodified`` () =
            invokeAction
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }
                { X = 5; Y = 0 }
                []
                EndOfLine
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" },
                { X = 5; Y = 0 },
                NotRequired
            )

        [<Fact>]
        let ``should return no change with pos modified when NonSearch`` () =
            invokeAction
                { state with
                    Query = "query"
                    PropertySearch = NoSearch }
                { X = 0; Y = 0 }
                []
                EndOfLine
            |> shouldEqual (
                { state with
                    Query = "query"
                    PropertySearch = NoSearch },
                { X = 5; Y = 0 },
                NotRequired
            )

    module ``with DeleteBackwardChar`` =
        [<Fact>]
        let ``should remove the character to the left of cursor, making state.Query one character shorter.`` () =
            invokeAction
                { state with
                    Query = ":name "
                    PropertySearch = NoSearch }
                { X = 6; Y = 0 }
                []
                DeleteBackwardChar
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" },
                { X = 5; Y = 0 },
                Required
            )

        [<Fact>]
        let ``should not change state if the cursor position is at the begin of line.`` () =
            invokeAction
                { state with
                    Query = ":name"
                    PropertySearch = NoSearch }
                { X = 0; Y = 0 }
                []
                DeleteBackwardChar
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = NoSearch },
                { X = 0; Y = 0 },
                NotRequired
            )

    module ``with DeleteForwardChar`` =
        [<Fact>]
        let ``should remove the character to the right of cursor, making state.Query one character shorter.`` () =
            invokeAction
                { state with
                    Query = ":name "
                    PropertySearch = Search "name" }
                { X = 0; Y = 0 }
                []
                DeleteForwardChar
            |> shouldEqual (
                { state with
                    Query = "name "
                    PropertySearch = NoSearch },
                { X = 0; Y = 0 },
                Required
            )

        [<Fact>]
        let ``should not change state if the cursor position is at the end of line.`` () =
            invokeAction
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }
                { X = 5; Y = 0 }
                []
                DeleteForwardChar
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" },
                { X = 5; Y = 0 },
                NotRequired
            )

    module ``with KillBeginningOfLine`` =
        [<Fact>]
        let ``should remove all characters before the specified position.`` () =
            invokeAction { state with Query = "examplequery" } { X = 7; Y = 0 } [] KillBeginningOfLine
            |> shouldEqual ({ state with Query = "query" }, { X = 0; Y = 0 }, Required)

        [<Fact>]
        let ``should not change state if the cursor position is at the begin of line.`` () =
            invokeAction { state with Query = "query" } { X = 0; Y = 0 } [] KillBeginningOfLine
            |> shouldEqual ({ state with Query = "query" }, { X = 0; Y = 0 }, NotRequired)

    module ``with KillEndOfLine`` =
        [<Fact>]
        let ``should remove characters after the current cursor position.`` () =
            invokeAction { state with Query = "examplequery" } { X = 7; Y = 0 } [] KillEndOfLine
            |> shouldEqual ({ state with Query = "example" }, { X = 7; Y = 0 }, Required)

        [<Fact>]
        let ``should not change state if the cursor position is at the end of line.`` () =
            invokeAction { state with Query = "example" } { X = 7; Y = 0 } [] KillEndOfLine
            |> shouldEqual ({ state with Query = "example" }, { X = 7; Y = 0 }, NotRequired)

    let testStateOnly action state expected =
        invokeAction state position [] action
        |> shouldEqual (expected, position, Required)

    module ``with RotateMatcher`` =
        let test before after =
            testStateOnly
                RotateMatcher
                { state with InternalState.QueryState.Matcher = before }
                { state with InternalState.QueryState.Matcher = after }

        [<Fact>]
        let ``should switch EQ to LIKE.`` () = test EQ LIKE

        [<Fact>]
        let ``should switch LIKE to MATCH.`` () = test LIKE MATCH

        [<Fact>]
        let ``should switch MATCh to EQ.`` () = test MATCH EQ

    module ``with RotateOperator`` =
        let test before after =
            testStateOnly
                RotateOperator
                { state with InternalState.QueryState.Operator = before }
                { state with InternalState.QueryState.Operator = after }

        [<Fact>]
        let ``should switch NONE to OR.`` () = test NONE OR

        [<Fact>]
        let ``should switch OR to AND.`` () = test OR AND

        [<Fact>]
        let ``should switch AND to NONE.`` () = test AND NONE

    module ``with ToggleCaseSensitive`` =
        let test before after =
            testStateOnly
                ToggleCaseSensitive
                { state with InternalState.QueryState.CaseSensitive = before }
                { state with InternalState.QueryState.CaseSensitive = after }

        [<Fact>]
        let ``should return a enabled case sensitive.`` () = test false true

        [<Fact>]
        let ``should return a disabled case sensitive.`` () = test true false

    module ``with ToggleInvertFilter`` =
        let test before after () =
            testStateOnly
                ToggleInvertFilter
                { state with InternalState.QueryState.Invert = before }
                { state with InternalState.QueryState.Invert = after }

        [<Fact>]
        let ``should return a enabled invert filter.`` () = test false true

        [<Fact>]
        let ``should return a disabled invert filter.`` () = test true false

    module ``with ToggleSuppressProperties`` =
        let test before after () =
            testStateOnly
                ToggleSuppressProperties
                { state with SuppressProperties = before }
                { state with SuppressProperties = after }

        [<Fact>]
        let ``should return a enabled suppress property.`` () = test false true

        [<Fact>]
        let ``should return a disabled suppress property.`` () = test true false

    let noop action =
        invokeAction state position [] action
        |> shouldEqual (state, position, NotRequired) // TODO: change to false when the action is implemented.

    module ``with SelectUp`` =
        [<Fact>]
        let ``shouldn't return any difference when a up-arrow is entered.`` () = noop SelectUp

    module ``with SelectDown`` =
        [<Fact>]
        let ``shouldn't return any difference when a down-arrow is entered.`` () = noop SelectDown

    module ``with ScrollPageUp`` =
        [<Fact>]
        let ``shouldn't return any difference when a page-up is entered.`` () = noop ScrollPageUp

    module ``with ScrollPageDown`` =
        [<Fact>]
        let ``shouldn't return any difference when a page-down is entered.`` () = noop ScrollPageDown

    module ``with TabExpansion`` =
        [<Fact>]
        let ``shouldn't return any difference when a tab is entered with non search mode.`` () =
            invokeAction state position [ "name"; "path" ] CompleteProperty
            |> shouldEqual (state, position, NotRequired)

        [<Fact>]
        let ``shouldn't return any difference when a tab is entered with empty properties list.`` () =
            let state =
                { state with
                    Query = ":"
                    PropertySearch = Search "" }

            invokeAction state position [] CompleteProperty
            |> shouldEqual (state, position, NotRequired)

        [<Fact>]
        let ``shouldn't return any difference when a tab is entered and found no property completion.`` () =
            let state =
                { state with
                    Query = ":a"
                    PropertySearch = Search "a" }

            invokeAction state position [ "name"; "path" ] CompleteProperty
            |> shouldEqual (state, position, NotRequired)

        [<Fact>]
        let ``should return completion when a property is found.`` () =
            let state =
                { state with
                    Query = ":p"
                    PropertySearch = Search "p" }

            let position = { position with X = 2 }

            invokeAction state position [ "name"; "path" ] CompleteProperty
            |> shouldEqual (
                { state with
                    Query = ":path"
                    PropertySearch = Rotate("p", 0, [ "path" ]) },
                { position with X = 5 },
                Required
            )

        [<Fact>]
        let ``should return completion when some properties are found.`` () =
            let state =
                { state with
                    Query = ":n"
                    PropertySearch = Search "n" }

            let position = { position with X = 2 }

            invokeAction state position [ "name"; "path"; "number" ] CompleteProperty
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Rotate("n", 0, [ "name"; "number" ]) },
                { position with X = 5 },
                Required
            )

        [<Fact>]
        let ``should insert completion to mid of query when a property is found.`` () =
            let state =
                { state with
                    Query = ":n foo"
                    PropertySearch = Search "n" }

            let position = { position with X = 2 }

            invokeAction state position [ "name"; "path" ] CompleteProperty
            |> shouldEqual (
                { state with
                    Query = ":name foo"
                    PropertySearch = Rotate("n", 0, [ "name" ]) },
                { position with X = 5 },
                Required
            )

        [<Fact>]
        let ``shouldn't return any difference when a property is already completed.`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }

            let position = { position with X = 5 }

            invokeAction state position [ "name"; "path" ] CompleteProperty
            |> shouldEqual ({ state with PropertySearch = Rotate("name", 0, [ "name" ]) }, position, Required)

        [<Fact>]
        let ``shouldn't return any difference when a property is already completed to mid of query.`` () =
            let state =
                { state with
                    Query = ":name a"
                    PropertySearch = Search "name" }

            let position = { position with X = 5 }

            invokeAction state position [ "name"; "path" ] CompleteProperty
            |> shouldEqual ({ state with PropertySearch = Rotate("name", 0, [ "name" ]) }, position, Required)

        [<Fact>]
        let ``should return next property when rotation.`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = Rotate("n", 0, [ "name"; "number" ]) }

            let position = { position with X = 5 }

            invokeAction state position [ "name"; "path"; "number" ] CompleteProperty
            |> shouldEqual (
                { state with
                    Query = ":number"
                    PropertySearch = Rotate("n", 1, [ "name"; "number" ]) },
                { position with X = 7 },
                Required
            )

        [<Fact>]
        let ``should return first property when next rotation not found.`` () =
            let state =
                { state with
                    Query = ":number"
                    PropertySearch = Rotate("n", 1, [ "name"; "number" ]) }

            let position = { position with X = 7 }

            invokeAction state position [ "name"; "path"; "number" ] CompleteProperty
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Rotate("n", 0, [ "name"; "number" ]) },
                { position with X = 5 },
                Required
            )
