module PocofTest.Handle

open Xunit
open FsUnitTyped

open Pocof
open Pocof.Data
open Pocof.Handle

module invokeAction =
    let state: InternalState =
        { QueryState =
            { Query = ""
              Cursor = 0
              WindowBeginningCursor = 0
              WindowWidth = 0
              InputMode = InputMode.Input }
          QueryCondition =
            { Matcher = Matcher.Match
              Operator = Operator.Or
              CaseSensitive = false
              Invert = false }
          PropertySearch = PropertySearch.NoSearch
          Notification = ""
          SuppressProperties = false
          Properties = []
          PropertyMap = Map []
          Prompt = "query"
          FilteredCount = 0
          ConsoleWidth = 60
          Refresh = Refresh.Required }
        |> InternalState.updateWindowWidth

    let position: Position = { Y = 0; Height = 20 }

    let noop action =
        let state, context = Query.prepare state

        invokeAction state position context action
        |> shouldEqual (
            { state with
                Refresh = Refresh.NotRequired },
            position,
            context
        )

    let testQueryEnd =
        function
        | [] -> ()
        | _ -> failwith "invalid query node end."

    let testQueryPartNormal value =
        function
        | [ Query.QueryPart.Normal(x) ] -> x value |> shouldEqual true
        | _ -> failwith "invalid query part normal."

    let testQueryPartProperty propertyName value =
        function
        | [ Query.QueryPart.Property(x, y) ] ->
            x |> shouldEqual propertyName
            y value |> shouldEqual true
        | _ -> failwith "invalid query part property."

    module ``with Noop`` =
        [<Fact>]
        let ``shouldn't return any difference.`` () = noop Action.Noop

    module ``with Cancel`` =
        [<Fact>]
        let ``should fail when unimplemented action is entered.`` () =
            let state, context = Query.prepare state

            shouldFail (fun () -> invokeAction state position context Action.Cancel |> ignore)

    module ``with AddQuery`` =
        [<Fact>]
        let ``should return a property search state and cursor=1 when the query is colon.`` () =
            let state = { state with Properties = [ "name" ] }
            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state position context (Action.AddQuery ":")

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":"
                    InternalState.QueryState.Cursor = 1
                    PropertySearch = PropertySearch.Search "" },
                position
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return a non-search state and cursor=6 when the query is space.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context (Action.AddQuery " ")

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = PropertySearch.NoSearch },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should remove the selection and add query.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 4
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state
            let pos = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context (Action.AddQuery "l")

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":l"
                    InternalState.QueryState.Cursor = 2
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "l" },
                pos
            )

            a3.Queries |> testQueryEnd

    module ``with BackwardChar`` =
        [<Fact>]
        let ``should return state with Refresh.NotRequired when moving forward on ':name' with cursor=0.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state

            let position: Position = { Y = 0; Height = 20 }

            let a1, a2, a3 = invokeAction state position context Action.BackwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    Refresh = Refresh.NotRequired },
                position
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return state with cursor=4 when moving forward on ':name' with cursor=5.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.BackwardChar

            (a1, a2)

            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 4
                    PropertySearch = PropertySearch.Search "nam" },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return state with cursor=4 and InputMode=Input when moving forward on ':name' with cursor=5 and InputMode=Select.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.BackwardChar

            (a1, a2)

            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 4
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "nam" },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

    module ``with ForwardChar`` =
        [<Fact>]
        let ``should return state with cursor=2 when moving forward on ':name' with cursor=1.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 1
                    PropertySearch = PropertySearch.Search "" }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.ForwardChar

            (a1, a2)

            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = PropertySearch.Search "n" },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return state with Refresh.NotRequired when moving forward on ':name' with cursor=5 and query.Length=3.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.ForwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name"
                    Refresh = Refresh.NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return state with cursor=2 and InputMode=Input when moving forward on ':name' with cursor=1 and InputMode=Select.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select -2
                    PropertySearch = PropertySearch.Search "" }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.ForwardChar

            (a1, a2)

            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 2
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "n" },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

    module ``with BackwardWord`` =
        [<Fact>]
        let ``should return state with Refresh.NotRequired when moving forward on ':name aaa ' with cursor=0.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state

            let position: Position = { Y = 0; Height = 20 }

            let a1, a2, a3 = invokeAction state position context Action.BackwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    Refresh = Refresh.NotRequired },
                position
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return state with cursor=1 when moving forward on ':name aaa ' with cursor=6.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.BackwardWord

            (a1, a2)

            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 1
                    PropertySearch = PropertySearch.Search "" },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return state with cursor=4 and InputMode=Input when moving forward on ':name aaa ' with cursor=5 and InputMode=Select.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Select 6
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.BackwardWord

            (a1, a2)

            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "" },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

    module ``with ForwardWord`` =
        [<Fact>]
        let ``should return state with cursor=5 when moving forward on ':name aaa ' with cursor=1.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 1
                    PropertySearch = PropertySearch.Search "" }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.ForwardWord

            (a1, a2)

            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = PropertySearch.NoSearch },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return state with Refresh.NotRequired when moving forward on ':name aaa ' with cursor=5 and query.Length=3.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 10
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.ForwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 10
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return state with cursor=4 and InputMode=Input when moving forward on ':name aaa ' with cursor=1 and InputMode=Select.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select -2
                    PropertySearch = PropertySearch.Search "" }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.ForwardWord

            (a1, a2)

            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.NoSearch },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

    module ``with BeginningOfLine`` =
        [<Fact>]
        let ``should return state with cursor=0.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.BeginningOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return state with Refresh.NotRequired.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.BeginningOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return state with cursor=0 and InputMode=Input.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.BeginningOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.NoSearch },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

    module ``with EndOfLine`` =
        [<Fact>]
        let ``should return state with cursor=query length.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context Action.EndOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return state with Refresh.NotRequired.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context Action.EndOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name"
                    Refresh = Refresh.NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return state with cursor=query length and InputMode=Input.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context Action.EndOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "name" },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

    module ``with DeleteBackwardChar`` =
        [<Fact>]
        let ``should remove the character to the left of cursor, making state.Query one character shorter.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.DeleteBackwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should not change state if the cursor position is at the begin of line.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.DeleteBackwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should correct state if the cursor position is over the query length.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ""
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.DeleteBackwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ""
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.Required },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should remove the selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Select 3
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state
            let pos = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.DeleteBackwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":na"
                    InternalState.QueryState.Cursor = 3
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "na" },
                pos
            )

            a3.Queries |> testQueryEnd

    module ``with DeleteForwardChar`` =
        [<Fact>]
        let ``should remove the character to the right of cursor, making state.Query one character shorter.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.DeleteForwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = "name "
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryPartNormal "name"

        [<Fact>]
        let ``should not change state if the cursor position is at the end of line.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.DeleteForwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name"
                    Refresh = Refresh.NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should correct state if the cursor position is over the query length.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.DeleteForwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name"
                    Refresh = Refresh.Required },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should remove the selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 3
                    InternalState.QueryState.InputMode = InputMode.Select -3
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state
            let pos = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.DeleteBackwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":na"
                    InternalState.QueryState.Cursor = 3
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "na" },
                pos
            )

            a3.Queries |> testQueryEnd

    module ``with DeleteBackwardWord`` =
        [<Fact>]
        let ``should remove the word to the left of cursor, making state.Query one word shorter.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.DeleteBackwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":pocof "
                    InternalState.QueryState.Cursor = 1
                    PropertySearch = PropertySearch.Search "" },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should not change state if the cursor position is at the begin of line.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.DeleteBackwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryPartProperty "name" "pocof"

        [<Fact>]
        let ``should correct state if the cursor position is over the query length.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ""
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.DeleteBackwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ""
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.Required },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should remove the forward selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Select -5
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state
            let pos = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.DeleteBackwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ": "
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "" },
                pos
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should remove the backward selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 11
                    InternalState.QueryState.InputMode = InputMode.Select 1
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state
            let pos = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.DeleteBackwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name  "
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.NoSearch },
                pos
            )

            a3.Queries |> testQueryEnd

    module ``with DeleteForwardWord`` =
        [<Fact>]
        let ``shouldn't return any difference when DeleteForwardWord is entered.`` () = noop Action.DeleteForwardWord

    module ``with KillBeginningOfLine`` =
        [<Fact>]
        let ``should remove all characters before the specified position.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.Cursor = 7 }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.DeleteBackwardInput

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = "query"
                    InternalState.QueryState.Cursor = 0 },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryPartNormal "query"

        [<Fact>]
        let ``should remove all characters when the cursor is over the query length.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.Cursor = 13 }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.DeleteBackwardInput

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ""
                    InternalState.QueryState.Cursor = 0 },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should not change state if the cursor position is at the begin of line.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "query"
                    InternalState.QueryState.Cursor = 0 }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.DeleteBackwardInput

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = "query"
                    InternalState.QueryState.Cursor = 0

                    Refresh = Refresh.NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryPartNormal "query"

        [<Fact>]
        let ``should remove all characters before the cursor including the selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    InternalState.QueryState.Cursor = 10 }

            let state, context = Query.prepare state
            let pos = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.DeleteBackwardInput

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = "ry"
                    InternalState.QueryState.InputMode = InputMode.Input
                    InternalState.QueryState.Cursor = 0 },
                pos
            )

            a3.Queries |> testQueryPartNormal "ry"

        [<Fact>]
        let ``should remove all characters before the cursor and the the selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.InputMode = InputMode.Select -5
                    InternalState.QueryState.Cursor = 5 }

            let state, context = Query.prepare state
            let pos = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.DeleteBackwardInput

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = "ry"
                    InternalState.QueryState.InputMode = InputMode.Input
                    InternalState.QueryState.Cursor = 0 },
                pos
            )

            a3.Queries |> testQueryPartNormal "ry"

    module ``with KillEndOfLine`` =
        [<Fact>]
        let ``should remove characters after the current cursor position.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.Cursor = 7 }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.DeleteForwardInput

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = "example"
                    InternalState.QueryState.Cursor = 7 },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryPartNormal "example"

        [<Fact>]
        let ``should not change state if the cursor position is at the end of line.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "example"
                    InternalState.QueryState.Cursor = 7 }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.DeleteForwardInput

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = "example"
                    InternalState.QueryState.Cursor = 7
                    Refresh = Refresh.NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryPartNormal "example"

        [<Fact>]
        let ``should remove all characters after the cursor including the selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    InternalState.QueryState.Cursor = 7 }

            let state, context = Query.prepare state
            let pos = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.DeleteForwardInput

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = "ex"
                    InternalState.QueryState.InputMode = InputMode.Input
                    InternalState.QueryState.Cursor = 2 },
                pos
            )

            a3.Queries |> testQueryPartNormal "ex"

        [<Fact>]
        let ``should remove all characters before the cursor and the the selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.InputMode = InputMode.Select -5
                    InternalState.QueryState.Cursor = 2 }

            let state, context = Query.prepare state
            let pos = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.DeleteForwardInput

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = "ex"
                    InternalState.QueryState.InputMode = InputMode.Input
                    InternalState.QueryState.Cursor = 2 },
                pos
            )

            a3.Queries |> testQueryPartNormal "ex"

    module ``with SelectBackwardChar`` =
        [<Fact>]
        let ``should return QueryState with no change when moving backward on ':name' with Cursor=0.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectBackwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    Refresh = Refresh.NotRequired },
                pos
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with Cursor=4 and InputMode=Select -1 when moving backward on ':name' with Cursor=5 and InputMode=Input.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectBackwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 4
                    InternalState.QueryState.InputMode = InputMode.Select(-1)
                    PropertySearch = PropertySearch.Search "nam" },
                pos
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with Cursor=3 and InputMode=Select -2 when moving backward on ':name' with Cursor=4 and InputMode=Select -1.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 4
                    InternalState.QueryState.InputMode = InputMode.Select(-1)
                    PropertySearch = PropertySearch.Search "nam" }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectBackwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 3
                    InternalState.QueryState.InputMode = InputMode.Select(-2)
                    PropertySearch = PropertySearch.Search "na" },
                pos
            )

            a3.Queries |> testQueryEnd

    module ``with SelectForwardChar`` =
        [<Fact>]
        let ``should return QueryState with Cursor=1 and InputMode=Select 1 when moving forward on ':name' with Cursor=0 and InputMode=Input.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.Search "" }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectForwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select 1
                    PropertySearch = PropertySearch.Search "" },
                pos
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with Cursor=1 and InputMode=Select 2 when moving forward on ':name' with Cursor=1 and InputMode=Select 1.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select 1
                    PropertySearch = PropertySearch.Search "" }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectForwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 2
                    InternalState.QueryState.InputMode = InputMode.Select 2
                    PropertySearch = PropertySearch.Search "n" },
                pos
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with no change when moving forward on ':name' with Cursor=5.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectForwardChar

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name"
                    Refresh = Refresh.NotRequired },
                pos
            )

            a3.Queries |> testQueryEnd

    module ``with SelectBackwardWord`` =
        [<Fact>]
        let ``should return QueryState with no change when moving backward with Cursor=0.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectBackwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    Refresh = Refresh.NotRequired },
                pos
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return QueryState with Cursor=1 and InputMode=Select -4 when moving backward with Cursor=5 and InputMode=Input.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectBackwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select(-4)
                    PropertySearch = PropertySearch.Search "" },
                pos
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return QueryState with Cursor=1 and InputMode=Select -4 when moving backward with Cursor=4 and InputMode=Select -1.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 4
                    InternalState.QueryState.InputMode = InputMode.Select(-1)
                    PropertySearch = PropertySearch.Search "nam" }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectBackwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select(-4)
                    PropertySearch = PropertySearch.Search "" },
                pos
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return QueryState with Cursor=0 and InputMode=Select -4 when moving backward with Cursor=4 and InputMode=Input.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 4
                    PropertySearch = PropertySearch.Search "nam" }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectBackwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select(-3)
                    PropertySearch = PropertySearch.Search "" },
                pos
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return QueryState with Cursor=0 and InputMode=Input when moving backward with Cursor=1 and InputMode=Select 1.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select 1
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectBackwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Cursor = 0
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.NoSearch },
                pos
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

    module ``with SelectForwardWord`` =
        [<Fact>]
        let ``should return QueryState with Cursor=1 and InputMode=Select 1 when moving forward with Cursor=0 and InputMode=Input.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectForwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select 1
                    PropertySearch = PropertySearch.Search "" },
                pos
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return QueryState with Cursor=6 and InputMode=Select 6 when moving forward with Cursor=1 and InputMode=Select 1.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select 1
                    PropertySearch = PropertySearch.Search "" }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectForwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Select 6
                    PropertySearch = PropertySearch.NoSearch },
                pos
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return QueryState with Cursor=6 and InputMode=Select 4 when moving forward with Cursor=2 and InputMode=Input.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = PropertySearch.Search "" }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectForwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Select 4
                    PropertySearch = PropertySearch.NoSearch },
                pos
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return QueryState with no change when moving forward with Cursor=10.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 10
                    InternalState.QueryState.InputMode = InputMode.Select 10 }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectForwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    Refresh = Refresh.NotRequired },
                pos
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return QueryState with Cursor=10 and InputMode=Input when moving forward with Cursor=6 and InputMode=Select 4.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Select -4 }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectForwardWord

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Cursor = 10
                    InternalState.QueryState.InputMode = InputMode.Input },
                pos
            )

            a3.Queries |> testQueryPartProperty "name" "aaa"

    module ``with SelectToBeginningOfLine`` =
        [<Fact>]
        let ``should return QueryState with Cursor=0 and InputMode=Select -5 when moving head on ':name' with Cursor=5 and InputMode=Input.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectToBeginningOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    InternalState.QueryState.InputMode = InputMode.Select -5
                    PropertySearch = PropertySearch.NoSearch },
                pos
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with Cursor=0 and InputMode=Select -5 when moving head on ':name' with Cursor=4 and InputMode=Select -1.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 4
                    InternalState.QueryState.InputMode = InputMode.Select -1
                    PropertySearch = PropertySearch.Search "nam" }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectToBeginningOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    InternalState.QueryState.InputMode = InputMode.Select -5
                    PropertySearch = PropertySearch.NoSearch },
                pos
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with no change when moving head on ':name' with Cursor=0.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectToBeginningOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.NotRequired },
                pos
            )

            a3.Queries |> testQueryEnd

    module ``with SelectToEndOfLine`` =
        [<Fact>]
        let ``should return QueryState with Cursor=5 and InputMode=Select 5 when moving tail on ':name' with Cursor=0 and InputMode=Input.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectToEndOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name" },
                pos
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with Cursor=5 and InputMode=Select 5 when moving tail on ':name' with Cursor=1 and InputMode=Select 1.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select 1
                    PropertySearch = PropertySearch.Search "" }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectToEndOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name" },
                pos
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with no change when moving tail on ':name' with Cursor=5.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state

            let a1, a2, a3 =
                invokeAction state { Y = 0; Height = 20 } context Action.SelectToEndOfLine

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name"
                    Refresh = Refresh.NotRequired },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

    module ``with SelectAll`` =
        [<Fact>]
        let ``should return QueryState with Cursor=5 and InputMode=Select 5 with Cursor=0 and InputMode=Input.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectAll

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name" },
                pos
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with Cursor=5 and InputMode=Select 5 with Cursor=1 and InputMode=Input.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "" }

            let state, context = Query.prepare state
            let pos: Position = { Y = 0; Height = 20 }
            let a1, a2, a3 = invokeAction state pos context Action.SelectAll

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name" },
                pos
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with Cursor=5 and InputMode=Select 5 with Cursor=5.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state { Y = 0; Height = 20 } context Action.SelectAll

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name" },
                { Y = 0; Height = 20 }
            )

            a3.Queries |> testQueryEnd

    let testStateAndContext action state context expectedState =
        let a1, a2, a3 = invokeAction state position context action

        (a1, a2) |> shouldEqual (expectedState, position)

        (a1, a2, a3)

    module ``with RotateMatcher`` =
        let test before after =
            let a =
                match before, after with
                | Matcher.Eq, Matcher.Like -> 2
                | Matcher.Like, Matcher.Match -> 1
                | Matcher.Match, Matcher.Eq -> -3
                | _ -> failwith "invalid case in RotateMatcher test."

            let stateBefore =
                { state with
                    InternalState.QueryCondition.Matcher = before }
                |> InternalState.updateWindowWidth

            let state, context = Query.prepare stateBefore

            let stateAfter =
                { state with
                    InternalState.QueryCondition.Matcher = after
                    InternalState.QueryState.WindowWidth = state.QueryState.WindowWidth - a }

            testStateAndContext Action.RotateMatcher stateBefore context stateAfter
        // TODO: test a3

        [<Fact>]
        let ``should switch EQ to LIKE.`` () = test Matcher.Eq Matcher.Like

        [<Fact>]
        let ``should switch LIKE to MATCH.`` () = test Matcher.Like Matcher.Match

        [<Fact>]
        let ``should switch MATCh to EQ.`` () = test Matcher.Match Matcher.Eq

    module ``with RotateOperator`` =
        let test before after =
            let a =
                match before, after with
                | Operator.Or, Operator.And -> 1
                | Operator.And, Operator.Or -> -1
                | _ -> failwith "invalid case in RotateOperator test."

            let stateBefore =
                { state with
                    InternalState.QueryCondition.Operator = before }
                |> InternalState.updateWindowWidth

            let state, context = Query.prepare stateBefore

            let stateAfter =
                { state with
                    InternalState.QueryCondition.Operator = after
                    InternalState.QueryState.WindowWidth = state.QueryState.WindowWidth - a }

            let _, _, a3 =
                testStateAndContext Action.RotateOperator stateBefore context stateAfter

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should switch OR to AND.`` () = test Operator.Or Operator.And

        [<Fact>]
        let ``should switch AND to OR.`` () = test Operator.And Operator.Or

    module ``with ToggleCaseSensitive`` =
        let test before after =
            let a =
                match after with
                | true -> 1
                | _ -> -1

            let stateBefore =
                { state with
                    InternalState.QueryCondition.CaseSensitive = before }
                |> InternalState.updateWindowWidth

            let state, context = Query.prepare stateBefore

            let stateAfter =
                { state with
                    InternalState.QueryCondition.CaseSensitive = after
                    InternalState.QueryState.WindowWidth = state.QueryState.WindowWidth - a }

            testStateAndContext Action.ToggleCaseSensitive stateBefore context stateAfter
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
                { state with
                    InternalState.QueryCondition.Invert = before }
                |> InternalState.updateWindowWidth

            let state, context = Query.prepare stateBefore

            let stateAfter =
                { state with
                    InternalState.QueryCondition.Invert = after
                    InternalState.QueryState.WindowWidth = state.QueryState.WindowWidth - a }

            testStateAndContext Action.ToggleInvertFilter stateBefore context stateAfter
        // TODO: test a3

        [<Fact>]
        let ``should return a enabled invert filter.`` () = test false true

        [<Fact>]
        let ``should return a disabled invert filter.`` () = test true false

    module ``with ToggleSuppressProperties`` =
        let test before after =
            let stateBefore =
                { state with
                    InternalState.SuppressProperties = before }

            let state, context = Query.prepare stateBefore

            let stateAfter =
                { state with
                    InternalState.SuppressProperties = after }

            testStateAndContext Action.ToggleSuppressProperties stateBefore context stateAfter
        // TODO: test a3

        [<Fact>]
        let ``should return a enabled suppress property.`` () = test false true

        [<Fact>]
        let ``should return a disabled suppress property.`` () = test true false

    module ``with SelectLineUp`` =
        [<Fact>]
        let ``shouldn't return any difference when SelectLineUp is entered.`` () = noop Action.SelectLineUp

    module ``with SelectLineDown`` =
        [<Fact>]
        let ``shouldn't return any difference when SelectLineDown is entered.`` () = noop Action.SelectLineDown

    module ``with ScrollPageUp`` =
        [<Fact>]
        let ``shouldn't return any difference when ScrollPageUp is entered.`` () = noop Action.ScrollPageUp

    module ``with ScrollPageDown`` =
        [<Fact>]
        let ``shouldn't return any difference when ScrollPageDown is entered.`` () = noop Action.ScrollPageDown

    module ``with CompleteProperty`` =
        [<Fact>]
        let ``shouldn't return any difference when a tab is entered with non search mode.`` () =
            let state =
                { state with
                    Properties = [ "name"; "path" ] }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state position context Action.CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    Refresh = Refresh.NotRequired },
                position
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``shouldn't return any difference when a tab is entered with empty properties list.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":"
                    PropertySearch = PropertySearch.Search "" }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state position context Action.CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    Refresh = Refresh.NotRequired },
                position
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``shouldn't return any difference when a tab is entered and found no property completion.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":a"
                    PropertySearch = PropertySearch.Search "a"
                    Properties = [ "name"; "path" ] }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state position context Action.CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    Refresh = Refresh.NotRequired },
                position
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return the first completion when a empty keyword.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":"
                    InternalState.QueryState.Cursor = 1
                    PropertySearch = PropertySearch.Search ""
                    Properties = [ "first"; "second"; "third" ] }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state position context Action.CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":first"
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = PropertySearch.Rotate("", 0, [ "first"; "second"; "third" ]) },
                position
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return completion when a property is found.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":p"
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = PropertySearch.Search "p"
                    Properties = [ "name"; "path" ] }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state position context Action.CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":path"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Rotate("p", 0, [ "path" ]) },
                position
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return completion when some properties are found.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":n"
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = PropertySearch.Search "n"
                    Properties = [ "name"; "path"; "number" ] }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state position context Action.CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Rotate("n", 0, [ "name"; "number" ]) },
                position
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should insert completion to mid of query when a property is found.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":n foo"
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = PropertySearch.Search "n"
                    Properties = [ "name"; "path" ] }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state position context Action.CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name foo"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Rotate("n", 0, [ "name" ]) },
                position
            )

            a3.Queries |> testQueryPartProperty "name" "foo"

        [<Fact>]
        let ``shouldn't return any difference when a property is already completed.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name"
                    Properties = [ "name"; "path" ] }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state position context Action.CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    PropertySearch = PropertySearch.Rotate("name", 0, [ "name" ]) },
                position
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``shouldn't return any difference when a property is already completed to mid of query.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name a"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name"
                    Properties = [ "name"; "path" ] }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state position context Action.CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    PropertySearch = PropertySearch.Rotate("name", 0, [ "name" ]) },
                position
            )

            a3.Queries |> testQueryPartProperty "name" "a"

        [<Fact>]
        let ``should return current completion when a property is already completed and cursor in mid position of it.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name a"
                    InternalState.QueryState.Cursor = 4
                    PropertySearch = PropertySearch.Search "nam"
                    Properties = [ "name"; "path" ] }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state position context Action.CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Rotate("nam", 0, [ "name" ]) },
                position
            )

            a3.Queries |> testQueryPartProperty "name" "a"

        [<Fact>]
        let ``should return next property when rotation.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Rotate("n", 0, [ "name"; "number" ])
                    Properties = [ "name"; "path"; "number" ] }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state position context Action.CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":number"
                    InternalState.QueryState.Cursor = 7
                    PropertySearch = PropertySearch.Rotate("n", 1, [ "name"; "number" ]) },
                position
            )

            a3.Queries |> testQueryEnd

        [<Fact>]
        let ``should return first property when next rotation not found.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":number"
                    InternalState.QueryState.Cursor = 7
                    PropertySearch = PropertySearch.Rotate("n", 1, [ "name"; "number" ])
                    Properties = [ "name"; "path"; "number" ] }

            let state, context = Query.prepare state

            let a1, a2, a3 = invokeAction state position context Action.CompleteProperty

            (a1, a2)
            |> shouldEqual (
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Rotate("n", 0, [ "name"; "number" ]) },
                position
            )

            a3.Queries |> testQueryEnd
