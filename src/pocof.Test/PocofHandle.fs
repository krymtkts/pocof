module PocofHandle

open Xunit
open FsUnitTyped
open pocof
open PocofData
open PocofHandle

module invokeAction =
    let state: InternalState =
        { QueryState =
            { Query = ""
              Cursor = 0
              WindowBeginningX = 0
              WindowWidth = 0 }
          QueryCondition =
            { Matcher = MATCH
              Operator = OR
              CaseSensitive = false
              Invert = false }
          PropertySearch = NoSearch
          Notification = ""
          SuppressProperties = false
          Properties = []
          Prompt = "query"
          FilteredCount = 0
          ConsoleWidth = 60
          Refresh = Required }
        |> InternalState.updateWindowWidth

    let position: Position = { Y = 0; Height = 20 }

    module ``with AddQuery`` =
        [<Fact>]
        let ``should return a property search state and position.x = 1 when the char is colon.`` () =
            let state = { state with Properties = [ "name" ] }
            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state position context (AddQuery ":")

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":"
                    InternalState.QueryState.Cursor = 1
                    PropertySearch = Search "" },
                position
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return a non-search state and position.X = 6 when the char is space.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Search "name" }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context (AddQuery " ")

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = NoSearch },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> shouldEqual []

    module ``with BackwardChar`` =
        [<Fact>]
        let ``should return state with pos unmodified when moving forward on ':name' with position.X=0.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = NoSearch }

            let state, context = PocofQuery.prepare state

            let position: Position = { Y = 0; Height = 20 }

            let a1, a2, a3 = invokeAction state position context BackwardChar

            (a1, a2)
            |> shouldEqual ({ state with Refresh = NotRequired }, position)

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return state with position.X=4 when moving forward on ':name' with position.X=5.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Search "name" }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context BackwardChar

            (a1, a2)

            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 4
                    PropertySearch = Search "nam" },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> shouldEqual []

    module ``with ForwardChar`` =
        [<Fact>]
        let ``should return state with position.X=2 when moving forward on ':name' with position.X=1.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 1
                    PropertySearch = Search "" }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context ForwardChar

            (a1, a2)

            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = Search "n" },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return state with pos unmodified when moving forward on ':name' with position.X=5 and query.Length=3.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Search "name" }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context ForwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Search "name"
                    Refresh = NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> shouldEqual []

    module ``with BeginningOfLine`` =
        [<Fact>]
        let ``should return state with position.X = 0.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Search "name" }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context BeginningOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = NoSearch },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return state with pos unmodified`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = NoSearch }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context BeginningOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = NoSearch
                    Refresh = NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> shouldEqual []

    module ``with EndOfLine`` =
        [<Fact>]
        let ``should return state with position.X = query length.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = NoSearch }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context EndOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Search "name" },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return state with pos unmodified`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Search "name" }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context EndOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Search "name"
                    Refresh = NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> shouldEqual []

    module ``with DeleteBackwardChar`` =
        [<Fact>]
        let ``should remove the character to the left of cursor, making state.Query one character shorter.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = NoSearch }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context DeleteBackwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Search "name" },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should not change state if the cursor position is at the begin of line.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = NoSearch }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context DeleteBackwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = NoSearch
                    Refresh = NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should correct state if the cursor position is over the query length.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ""
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = NoSearch }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context DeleteBackwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ""
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = NoSearch
                    Refresh = Required },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> shouldEqual [ PocofQuery.Normal "" ] // TODO: Should be an empty list, though harmless.

    module ``with DeleteForwardChar`` =
        [<Fact>]
        let ``should remove the character to the right of cursor, making state.Query one character shorter.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = Search "name" }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context DeleteForwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = "name "
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = NoSearch },
                { Y = 0; Height = 20 }
            )

            a3.Queries
            |> shouldEqual [ PocofQuery.Normal "name" ]

        [<Fact>]
        let ``should not change state if the cursor position is at the end of line.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Search "name" }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context DeleteForwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Search "name"
                    Refresh = NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should correct state if the cursor position is over the query length.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = Search "name" }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context DeleteForwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Search "name"
                    Refresh = Required },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> shouldEqual []

    module ``with KillBeginningOfLine`` =
        [<Fact>]
        let ``should remove all characters before the specified position.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.Cursor = 7 }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context KillBeginningOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = "query"
                    InternalState.QueryState.Cursor = 0 },
                { Y = 0; Height = 20 }
            )

            a3.Queries
            |> shouldEqual [ PocofQuery.Normal("query") ]

        [<Fact>]
        let ``should remove all characters when the cursor is over the query length.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.Cursor = 13 }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context KillBeginningOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ""
                    InternalState.QueryState.Cursor = 0 },
                { Y = 0; Height = 20 }
            )

            a3.Queries
            |> shouldEqual [ PocofQuery.Normal("") ]

        [<Fact>]
        let ``should not change state if the cursor position is at the begin of line.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "query"
                    InternalState.QueryState.Cursor = 0 }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context KillBeginningOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = "query"
                    InternalState.QueryState.Cursor = 0

                    Refresh = NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries
            |> shouldEqual [ PocofQuery.Normal("query") ]

    module ``with KillEndOfLine`` =
        [<Fact>]
        let ``should remove characters after the current cursor position.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.Cursor = 7 }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context KillEndOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = "example"
                    InternalState.QueryState.Cursor = 7 },
                { Y = 0; Height = 20 }
            )

            a3.Queries
            |> shouldEqual [ PocofQuery.Normal("example") ]

        [<Fact>]
        let ``should not change state if the cursor position is at the end of line.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "example"
                    InternalState.QueryState.Cursor = 7 }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context KillEndOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = "example"
                    InternalState.QueryState.Cursor = 7
                    Refresh = NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries
            |> shouldEqual [ PocofQuery.Normal("example") ]

    let testStateAndContext action state context expectedState =
        let a1, a2, a3 = invokeAction state position context action

        (a1, a2) |> shouldEqual (expectedState, position)

        (a1, a2, a3)

    module ``with RotateMatcher`` =
        let test before after =
            let a =
                match before, after with
                | EQ, LIKE -> 2
                | LIKE, MATCH -> 1
                | MATCH, EQ -> -3
                | _ -> failwith "invalid case in this test."

            let stateBefore =
                { state with InternalState.QueryCondition.Matcher = before }
                |> InternalState.updateWindowWidth

            let state, context = PocofQuery.prepare stateBefore

            let stateAfter =
                { state with
                    InternalState.QueryCondition.Matcher = after
                    InternalState.QueryState.WindowWidth = state.QueryState.WindowWidth - a }

            testStateAndContext RotateMatcher stateBefore context stateAfter
        // TODO: test a3

        [<Fact>]
        let ``should switch EQ to LIKE.`` () = test EQ LIKE

        [<Fact>]
        let ``should switch LIKE to MATCH.`` () = test LIKE MATCH

        [<Fact>]
        let ``should switch MATCh to EQ.`` () = test MATCH EQ

    module ``with RotateOperator`` =
        let test before after =
            let a =
                match before, after with
                | NONE, OR -> -2
                | OR, AND -> 1
                | AND, NONE -> 1
                | _ -> failwith "invalid case in this test."

            let stateBefore =
                { state with InternalState.QueryCondition.Operator = before }
                |> InternalState.updateWindowWidth

            let state, context = PocofQuery.prepare stateBefore

            let stateAfter =
                { state with
                    InternalState.QueryCondition.Operator = after
                    InternalState.QueryState.WindowWidth = state.QueryState.WindowWidth - a }

            let _, _, a3 = testStateAndContext RotateOperator stateBefore context stateAfter

            a3.Queries
            |> shouldEqual [ PocofQuery.Normal("") ]

        [<Fact>]
        let ``should switch NONE to OR.`` () = test NONE OR

        [<Fact>]
        let ``should switch OR to AND.`` () = test OR AND

        [<Fact>]
        let ``should switch AND to NONE.`` () = test AND NONE

    module ``with ToggleCaseSensitive`` =
        let test before after =
            let a =
                match after with
                | true -> 1
                | _ -> -1

            let stateBefore =
                { state with InternalState.QueryCondition.CaseSensitive = before }
                |> InternalState.updateWindowWidth

            let state, context = PocofQuery.prepare stateBefore

            let stateAfter =
                { state with
                    InternalState.QueryCondition.CaseSensitive = after
                    InternalState.QueryState.WindowWidth = state.QueryState.WindowWidth - a }

            testStateAndContext ToggleCaseSensitive stateBefore context stateAfter
        // TODO: test a3

        [<Fact>]
        let ``should return a enabled case sensitive.`` () = test false true

        [<Fact>]
        let ``should return a disabled case sensitive.`` () = test true false

    module ``with ToggleInvertFilter`` =
        let test before after =
            let a =
                match after with
                | true -> 3
                | _ -> -3

            let stateBefore =
                { state with InternalState.QueryCondition.Invert = before }
                |> InternalState.updateWindowWidth

            let state, context = PocofQuery.prepare stateBefore

            let stateAfter =
                { state with
                    InternalState.QueryCondition.Invert = after
                    InternalState.QueryState.WindowWidth = state.QueryState.WindowWidth - a }

            testStateAndContext ToggleInvertFilter stateBefore context stateAfter
        // TODO: test a3

        [<Fact>]
        let ``should return a enabled invert filter.`` () = test false true

        [<Fact>]
        let ``should return a disabled invert filter.`` () = test true false

    module ``with ToggleSuppressProperties`` =
        let test before after =
            let stateBefore = { state with InternalState.SuppressProperties = before }
            let state, context = PocofQuery.prepare stateBefore
            let stateAfter = { state with InternalState.SuppressProperties = after }

            testStateAndContext ToggleSuppressProperties stateBefore context stateAfter
        // TODO: test a3

        [<Fact>]
        let ``should return a enabled suppress property.`` () = test false true

        [<Fact>]
        let ``should return a disabled suppress property.`` () = test true false

    let noop action =
        let state, context = PocofQuery.prepare state

        let a1, a2, a3 = invokeAction state position context action

        (a1, a2, a3)
        |> shouldEqual ({ state with Refresh = NotRequired }, position, context)

    module ``with SelectLineUp`` =
        [<Fact>]
        let ``shouldn't return any difference when a up-arrow is entered.`` () = noop SelectLineUp

    module ``with SelectLineDown`` =
        [<Fact>]
        let ``shouldn't return any difference when a down-arrow is entered.`` () = noop SelectLineDown

    module ``with ScrollPageUp`` =
        [<Fact>]
        let ``shouldn't return any difference when a page-up is entered.`` () = noop ScrollPageUp

    module ``with ScrollPageDown`` =
        [<Fact>]
        let ``shouldn't return any difference when a page-down is entered.`` () = noop ScrollPageDown

    module ``with CompleteProperty`` =
        [<Fact>]
        let ``shouldn't return any difference when a tab is entered with non search mode.`` () =
            let state = { state with Properties = [ "name"; "path" ] }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state position context CompleteProperty

            (a1, a2)
            |> shouldEqual ({ state with Refresh = NotRequired }, position)

            a3.Queries
            |> shouldEqual [ PocofQuery.Normal("") ]

        [<Fact>]
        let ``shouldn't return any difference when a tab is entered with empty properties list.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":"
                    PropertySearch = Search "" }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state position context CompleteProperty

            (a1, a2)
            |> shouldEqual ({ state with Refresh = NotRequired }, position)

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``shouldn't return any difference when a tab is entered and found no property completion.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":a"
                    PropertySearch = Search "a"
                    Properties = [ "name"; "path" ] }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state position context CompleteProperty

            (a1, a2)
            |> shouldEqual ({ state with Refresh = NotRequired }, position)

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return the first completion when a empty keyword.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":"
                    InternalState.QueryState.Cursor = 1
                    PropertySearch = Search ""
                    Properties = [ "first"; "second"; "third" ] }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state position context CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":first"
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = Rotate("", 0, [ "first"; "second"; "third" ]) },
                position
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return completion when a property is found.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":p"
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = Search "p"
                    Properties = [ "name"; "path" ] }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state position context CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":path"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Rotate("p", 0, [ "path" ]) },
                position
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return completion when some properties are found.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":n"
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = Search "n"
                    Properties = [ "name"; "path"; "number" ] }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state position context CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Rotate("n", 0, [ "name"; "number" ]) },
                position
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should insert completion to mid of query when a property is found.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":n foo"
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = Search "n"
                    Properties = [ "name"; "path" ] }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state position context CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name foo"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Rotate("n", 0, [ "name" ]) },
                position
            )

            a3.Queries
            |> shouldEqual [ PocofQuery.Property("name", "foo") ]

        [<Fact>]
        let ``shouldn't return any difference when a property is already completed.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Search "name"
                    Properties = [ "name"; "path" ] }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state position context CompleteProperty

            (a1, a2)
            |> shouldEqual ({ state with PropertySearch = Rotate("name", 0, [ "name" ]) }, position)

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``shouldn't return any difference when a property is already completed to mid of query.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name a"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Search "name"
                    Properties = [ "name"; "path" ] }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state position context CompleteProperty

            (a1, a2)
            |> shouldEqual ({ state with PropertySearch = Rotate("name", 0, [ "name" ]) }, position)

            a3.Queries
            |> shouldEqual [ PocofQuery.Property("name", "a") ]

        [<Fact>]
        let ``should return current completion when a property is already completed and cursor in mid position of it.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name a"
                    InternalState.QueryState.Cursor = 4
                    PropertySearch = Search "nam"
                    Properties = [ "name"; "path" ] }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state position context CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Rotate("nam", 0, [ "name" ]) },
                position
            )

            a3.Queries
            |> shouldEqual [ PocofQuery.Property("name", "a") ]

        [<Fact>]
        let ``should return next property when rotation.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Rotate("n", 0, [ "name"; "number" ])
                    Properties = [ "name"; "path"; "number" ] }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state position context CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":number"
                    InternalState.QueryState.Cursor = 7
                    PropertySearch = Rotate("n", 1, [ "name"; "number" ]) },
                position
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return first property when next rotation not found.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":number"
                    InternalState.QueryState.Cursor = 7
                    PropertySearch = Rotate("n", 1, [ "name"; "number" ])
                    Properties = [ "name"; "path"; "number" ] }

            let state, context = PocofQuery.prepare state

            let a1, a2, a3 = invokeAction state position context CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = Rotate("n", 0, [ "name"; "number" ]) },
                position
            )

            a3.Queries |> shouldEqual []
