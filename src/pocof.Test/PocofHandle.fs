module PocofHandle

open Xunit
open FsUnitTyped
open pocof
open PocofData
open PocofHandle

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
            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state { X = 0; Y = 0 } context [ "name" ] (AddQuery ":")

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = ":"
                    PropertySearch = Search "" },
                { X = 1; Y = 0 },
                Required
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return a non-search state and position.X = 6 when the char is space.`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 = invokeAction state { X = 5; Y = 0 } context [] (AddQuery " ")

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = ":name "
                    PropertySearch = NoSearch },
                { X = 6; Y = 0 },
                Required
            )

            a3.Queries |> shouldEqual []

    module ``with BackwardChar`` =
        [<Fact>]
        let ``should return state with pos unmodified when moving forward on ':name' with position.X=0.`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = NoSearch }

            let context = PocofQuery.prepare state
            let position: Position = { X = 0; Y = 0 }

            let a1, a2, a3, a4 = invokeAction state position context [] BackwardChar

            (a1, a2, a4)
            |> shouldEqual (state, position, NotRequired)

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return state with position.X=4 when moving forward on ':name' with position.X=5.`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 = invokeAction state { X = 5; Y = 0 } context [] BackwardChar

            (a1, a2, a4)

            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Search "nam" },
                { X = 4; Y = 0 },
                Required
            )

            a3.Queries |> shouldEqual []

    module ``with ForwardChar`` =
        [<Fact>]
        let ``should return state with position.X=2 when moving forward on ':name' with position.X=1.`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = Search "" }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 = invokeAction state { X = 1; Y = 0 } context [] ForwardChar

            (a1, a2, a4)

            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Search "n" },
                { X = 2; Y = 0 },
                Required
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return state with pos unmodified when moving forward on ':name' with position.X=5 and query.Length=3.``
            ()
            =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 = invokeAction state { X = 5; Y = 0 } context [] ForwardChar

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" },
                { X = 5; Y = 0 },
                NotRequired
            )

            a3.Queries |> shouldEqual []

    module ``with BeginningOfLine`` =
        [<Fact>]
        let ``should return state with position.X = 0.`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 = invokeAction state { X = 5; Y = 0 } context [] BeginningOfLine

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = NoSearch },
                { X = 0; Y = 0 },
                Required
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return state with pos unmodified`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 = invokeAction state { X = 0; Y = 0 } context [] BeginningOfLine

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = NoSearch },
                { X = 0; Y = 0 },
                NotRequired
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return no change with pos modified when NonSearch`` () =
            let state =
                { state with
                    Query = "query"
                    PropertySearch = NoSearch }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 = invokeAction state { X = 5; Y = 0 } context [] BeginningOfLine

            (a1, a2, a4)
            |> (shouldEqual (
                { state with
                    Query = "query"
                    PropertySearch = NoSearch },
                { X = 0; Y = 0 },
                NotRequired
            ))

            a3.Queries
            |> shouldEqual [ PocofQuery.Normal("query") ]

    module ``with EndOfLine`` =
        [<Fact>]
        let ``should return state with position.X = query length.`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = Search "n" }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 = invokeAction state { X = 2; Y = 0 } context [] EndOfLine

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" },
                { X = 5; Y = 0 },
                Required
            )

            a3.Queries |> shouldEqual []


        [<Fact>]
        let ``should return state with pos unmodified`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 = invokeAction state { X = 5; Y = 0 } context [] EndOfLine

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" },
                { X = 5; Y = 0 },
                NotRequired
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return no change with pos modified when NonSearch`` () =
            let state =
                { state with
                    Query = "query"
                    PropertySearch = NoSearch }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 = invokeAction state { X = 0; Y = 0 } context [] EndOfLine

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = "query"
                    PropertySearch = NoSearch },
                { X = 5; Y = 0 },
                NotRequired
            )

            a3.Queries
            |> shouldEqual [ PocofQuery.Normal("query") ]

    module ``with DeleteBackwardChar`` =
        [<Fact>]
        let ``should remove the character to the left of cursor, making state.Query one character shorter.`` () =
            let state =
                { state with
                    Query = ":name "
                    PropertySearch = NoSearch }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state { X = 6; Y = 0 } context [] DeleteBackwardChar

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" },
                { X = 5; Y = 0 },
                Required
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should not change state if the cursor position is at the begin of line.`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = NoSearch }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state { X = 0; Y = 0 } context [] DeleteBackwardChar

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = NoSearch },
                { X = 0; Y = 0 },
                NotRequired
            )

            a3.Queries |> shouldEqual []

    module ``with DeleteForwardChar`` =
        [<Fact>]
        let ``should remove the character to the right of cursor, making state.Query one character shorter.`` () =
            let state =
                { state with
                    Query = ":name "
                    PropertySearch = Search "name" }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state { X = 0; Y = 0 } context [] DeleteForwardChar

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = "name "
                    PropertySearch = NoSearch },
                { X = 0; Y = 0 },
                Required
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should not change state if the cursor position is at the end of line.`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state { X = 5; Y = 0 } context [] DeleteForwardChar

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" },
                { X = 5; Y = 0 },
                NotRequired
            )

            a3.Queries |> shouldEqual []

    module ``with KillBeginningOfLine`` =
        [<Fact>]
        let ``should remove all characters before the specified position.`` () =
            let state = { state with Query = "examplequery" }
            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state { X = 7; Y = 0 } context [] KillBeginningOfLine

            (a1, a2, a4)
            |> shouldEqual ({ state with Query = "query" }, { X = 0; Y = 0 }, Required)

            a3.Queries
            |> shouldEqual [ PocofQuery.Normal("query") ]

        [<Fact>]
        let ``should not change state if the cursor position is at the begin of line.`` () =
            let state = { state with Query = "query" }
            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state { X = 0; Y = 0 } context [] KillBeginningOfLine

            (a1, a2, a4)
            |> shouldEqual ({ state with Query = "query" }, { X = 0; Y = 0 }, NotRequired)

            a3.Queries
            |> shouldEqual [ PocofQuery.Normal("query") ]

    module ``with KillEndOfLine`` =
        [<Fact>]
        let ``should remove characters after the current cursor position.`` () =
            let state = { state with Query = "examplequery" }
            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 = invokeAction state { X = 7; Y = 0 } context [] KillEndOfLine

            (a1, a2, a4)
            |> shouldEqual ({ state with Query = "example" }, { X = 7; Y = 0 }, Required)

            a3.Queries
            |> shouldEqual [ PocofQuery.Normal("example") ]

        [<Fact>]
        let ``should not change state if the cursor position is at the end of line.`` () =
            let state = { state with Query = "example" }
            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 = invokeAction state { X = 7; Y = 0 } context [] KillEndOfLine

            (a1, a2, a4)
            |> shouldEqual ({ state with Query = "example" }, { X = 7; Y = 0 }, NotRequired)

            a3.Queries
            |> shouldEqual [ PocofQuery.Normal("example") ]

    let testStateAndContext action state context expectedState =
        let a1, a2, a3, a4 = invokeAction state position context [] action

        (a1, a2, a4)
        |> shouldEqual (expectedState, position, Required)

        (a1, a2, a3, a4)

    module ``with RotateMatcher`` =
        let test before after =
            let stateBefore = { state with InternalState.QueryState.Matcher = before }
            let context = PocofQuery.prepare stateBefore
            let stateAfter = { state with InternalState.QueryState.Matcher = after }

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
            let stateBefore = { state with InternalState.QueryState.Operator = before }
            let context = PocofQuery.prepare stateBefore
            let stateAfter = { stateBefore with InternalState.QueryState.Operator = after }

            let _, _, a3, _ = testStateAndContext RotateOperator stateBefore context stateAfter

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
            let stateBefore = { state with InternalState.QueryState.CaseSensitive = before }
            let context = PocofQuery.prepare stateBefore
            let stateAfter = { state with InternalState.QueryState.CaseSensitive = after }

            testStateAndContext ToggleCaseSensitive stateBefore context stateAfter
        // TODO: test a3

        [<Fact>]
        let ``should return a enabled case sensitive.`` () = test false true

        [<Fact>]
        let ``should return a disabled case sensitive.`` () = test true false

    module ``with ToggleInvertFilter`` =
        let test before after () =
            let stateBefore = { state with InternalState.QueryState.Invert = before }
            let context = PocofQuery.prepare stateBefore
            let stateAfter = { state with InternalState.QueryState.Invert = after }

            testStateAndContext ToggleInvertFilter stateBefore context stateAfter
        // TODO: test a3

        [<Fact>]
        let ``should return a enabled invert filter.`` () = test false true

        [<Fact>]
        let ``should return a disabled invert filter.`` () = test true false

    module ``with ToggleSuppressProperties`` =
        let test before after () =
            let stateBefore = { state with InternalState.QueryState.Invert = before }
            let context = PocofQuery.prepare stateBefore
            let stateAfter = { state with InternalState.QueryState.Invert = after }

            testStateAndContext ToggleSuppressProperties stateBefore context stateAfter
        // TODO: test a3

        [<Fact>]
        let ``should return a enabled suppress property.`` () = test false true

        [<Fact>]
        let ``should return a disabled suppress property.`` () = test true false

    let noop action =
        let context = PocofQuery.prepare state

        let a1, a2, a3, a4 = invokeAction state position context [] action

        (a1, a2, a3, a4)
        |> shouldEqual (state, position, context, NotRequired)

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
            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state position context [ "name"; "path" ] CompleteProperty

            (a1, a2, a4)
            |> shouldEqual (state, position, NotRequired)

            a3.Queries
            |> shouldEqual [ PocofQuery.Normal("") ]

        [<Fact>]
        let ``shouldn't return any difference when a tab is entered with empty properties list.`` () =
            let state =
                { state with
                    Query = ":"
                    PropertySearch = Search "" }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 = invokeAction state position context [] CompleteProperty

            (a1, a2, a4)
            |> shouldEqual (state, position, NotRequired)

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``shouldn't return any difference when a tab is entered and found no property completion.`` () =
            let state =
                { state with
                    Query = ":a"
                    PropertySearch = Search "a" }

            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state position context [ "name"; "path" ] CompleteProperty

            (a1, a2, a4)
            |> shouldEqual (state, position, NotRequired)

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return completion when a property is found.`` () =
            let state =
                { state with
                    Query = ":p"
                    PropertySearch = Search "p" }

            let position = { position with X = 2 }
            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state position context [ "name"; "path" ] CompleteProperty

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = ":path"
                    PropertySearch = Rotate("p", 0, [ "path" ]) },
                { position with X = 5 },
                Required
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return completion when some properties are found.`` () =
            let state =
                { state with
                    Query = ":n"
                    PropertySearch = Search "n" }

            let position = { position with X = 2 }
            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state position context [ "name"; "path"; "number" ] CompleteProperty

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Rotate("n", 0, [ "name"; "number" ]) },
                { position with X = 5 },
                Required
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should insert completion to mid of query when a property is found.`` () =
            let state =
                { state with
                    Query = ":n foo"
                    PropertySearch = Search "n" }

            let position = { position with X = 2 }
            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state position context [ "name"; "path" ] CompleteProperty

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = ":name foo"
                    PropertySearch = Rotate("n", 0, [ "name" ]) },
                { position with X = 5 },
                Required
            )

            a3.Queries
            |> shouldEqual [ PocofQuery.Property("name", "foo") ]

        [<Fact>]
        let ``shouldn't return any difference when a property is already completed.`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }

            let position = { position with X = 5 }
            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state position context [ "name"; "path" ] CompleteProperty

            (a1, a2, a4)
            |> shouldEqual ({ state with PropertySearch = Rotate("name", 0, [ "name" ]) }, position, Required)

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``shouldn't return any difference when a property is already completed to mid of query.`` () =
            let state =
                { state with
                    Query = ":name a"
                    PropertySearch = Search "name" }

            let position = { position with X = 5 }
            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state position context [ "name"; "path" ] CompleteProperty

            (a1, a2, a4)
            |> shouldEqual ({ state with PropertySearch = Rotate("name", 0, [ "name" ]) }, position, Required)

            a3.Queries
            |> shouldEqual [ PocofQuery.Property("name", "a") ]

        [<Fact>]
        let ``should return next property when rotation.`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = Rotate("n", 0, [ "name"; "number" ]) }

            let position = { position with X = 5 }
            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state position context [ "name"; "path"; "number" ] CompleteProperty

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = ":number"
                    PropertySearch = Rotate("n", 1, [ "name"; "number" ]) },
                { position with X = 7 },
                Required
            )

            a3.Queries |> shouldEqual []

        [<Fact>]
        let ``should return first property when next rotation not found.`` () =
            let state =
                { state with
                    Query = ":number"
                    PropertySearch = Rotate("n", 1, [ "name"; "number" ]) }

            let position = { position with X = 7 }
            let context = PocofQuery.prepare state

            let a1, a2, a3, a4 =
                invokeAction state position context [ "name"; "path"; "number" ] CompleteProperty

            (a1, a2, a4)
            |> shouldEqual (
                { state with
                    Query = ":name"
                    PropertySearch = Rotate("n", 0, [ "name"; "number" ]) },
                { position with X = 5 },
                Required
            )

            a3.Queries |> shouldEqual []
