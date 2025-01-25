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
          SuppressProperties = false
          Refresh = Refresh.Required }
        |> InternalState.updateConsoleWidth ("query>" |> String.length) 60

    // NOTE: for easier testing.
    let invokeAction = Handle.invokeAction ";:,.[]{}()/\\|!?^&*-=+'\"–—―"

    let noop action =
        let context, _ = Query.prepare state

        invokeAction [] state context action
        |> shouldEqual (
            { state with
                Refresh = Refresh.NotRequired },
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
            let context, _ = Query.prepare state
            shouldFail (fun () -> invokeAction [] state context Action.Cancel |> ignore)

    module ``with AddQuery`` =
        [<Fact>]
        let ``should return a property search state and cursor=1 when the query is colon.`` () =
            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [ "name" ] state context (Action.AddQuery ":")

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":"
                    InternalState.QueryState.Cursor = 1
                    PropertySearch = PropertySearch.Search "" }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return a non-search state and cursor=6 when the query is space.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [ "name" ] state context (Action.AddQuery " ")

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = PropertySearch.NoSearch }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should remove the selection and add query.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 4
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [ "name" ] state context (Action.AddQuery "l")

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":l"
                    InternalState.QueryState.Cursor = 2
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "l" }

            a2.Queries |> testQueryEnd

    module ``with BackwardChar`` =
        [<Fact>]
        let ``should return state with Refresh.NotRequired when moving forward on ':name' with cursor=0.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.BackwardChar

            a1
            |> shouldEqual
                { state with
                    Refresh = Refresh.NotRequired }


            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return state with cursor=4 when moving forward on ':name' with cursor=5.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.BackwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 4
                    PropertySearch = PropertySearch.Search "nam" }

            a2.Queries |> testQueryEnd

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

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.BackwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 4
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "nam" }

            a2.Queries |> testQueryEnd

    module ``with ForwardChar`` =
        [<Fact>]
        let ``should return state with cursor=2 when moving forward on ':name' with cursor=1.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 1
                    PropertySearch = PropertySearch.Search "" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.ForwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = PropertySearch.Search "n" }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return state with Refresh.NotRequired when moving forward on ':name' with cursor=5 and query.Length=3.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.ForwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name"
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryEnd

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

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.ForwardChar

            a1

            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 2
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "n" }

            a2.Queries |> testQueryEnd

    module ``with BackwardWord`` =
        [<Fact>]
        let ``should return state with Refresh.NotRequired when moving forward on ':name aaa ' with cursor=0.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.BackwardWord

            a1
            |> shouldEqual
                { state with
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return state with cursor=1 when moving forward on ':name aaa ' with cursor=6.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.BackwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 1
                    PropertySearch = PropertySearch.Search "" }

            a2.Queries |> testQueryPartProperty "name" "aaa"

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

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.BackwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "" }

            a2.Queries |> testQueryPartProperty "name" "aaa"

    module ``with ForwardWord`` =
        [<Fact>]
        let ``should return state with cursor=5 when moving forward on ':name aaa ' with cursor=1.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 1
                    PropertySearch = PropertySearch.Search "" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.ForwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = PropertySearch.NoSearch }

            a2.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return state with Refresh.NotRequired when moving forward on ':name aaa ' with cursor=5 and query.Length=3.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 10
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.ForwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 10
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryPartProperty "name" "aaa"

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

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.ForwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.NoSearch }

            a2.Queries |> testQueryPartProperty "name" "aaa"

    module ``with BeginningOfLine`` =
        [<Fact>]
        let ``should return state with cursor=0.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.BeginningOfLine

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return state with Refresh.NotRequired.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.BeginningOfLine

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return state with cursor=0 and InputMode=Input.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.BeginningOfLine

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.NoSearch }

            a2.Queries |> testQueryEnd

    module ``with EndOfLine`` =
        [<Fact>]
        let ``should return state with cursor=query length.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.EndOfLine

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return state with Refresh.NotRequired.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.EndOfLine

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name"
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return state with cursor=query length and InputMode=Input.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.EndOfLine

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "name" }

            a2.Queries |> testQueryEnd

    module ``with DeleteBackwardChar`` =
        [<Fact>]
        let ``should remove the character to the left of cursor, making state.Query one character shorter.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should not change state if the cursor is at the begin of line.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should correct state if the cursor is over the query length.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ""
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ""
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.Required }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should remove the selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Select 3
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":na"
                    InternalState.QueryState.Cursor = 3
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "na" }

            a2.Queries |> testQueryEnd

    module ``with DeleteForwardChar`` =
        [<Fact>]
        let ``should remove the character to the right of cursor, making state.Query one character shorter.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteForwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = "name "
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            a2.Queries |> testQueryPartNormal "name"

        [<Fact>]
        let ``should not change state if the cursor is at the end of line.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteForwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name"
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should correct state if the cursor is over the query length.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteForwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name"
                    Refresh = Refresh.Required }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should remove the selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 3
                    InternalState.QueryState.InputMode = InputMode.Select -3
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":na"
                    InternalState.QueryState.Cursor = 3
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "na" }

            a2.Queries |> testQueryEnd

    module ``with DeleteBackwardWord`` =
        [<Fact>]
        let ``should remove the word to the left of cursor, making state.Query one word shorter.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":pocof "
                    InternalState.QueryState.Cursor = 1
                    PropertySearch = PropertySearch.Search "" }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should not change state if the cursor is at the begin of line.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryPartProperty "name" "pocof"

        [<Fact>]
        let ``should correct state if the cursor is over the query length.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ""
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ""
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.Required }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should remove the forward selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Select -5
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ": "
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "" }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should remove the backward word larger than selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 11
                    InternalState.QueryState.InputMode = InputMode.Select 1
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name  "
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.NoSearch }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should remove the backward selection larger then word.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 11
                    InternalState.QueryState.InputMode = InputMode.Select 7
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":nam "
                    InternalState.QueryState.Cursor = 4
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "nam" }

            a2.Queries |> testQueryEnd

    module ``with DeleteForwardWord`` =
        [<Fact>]
        let ``should remove the word to the right of cursor, making state.Query one word shorter.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteForwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 6
                    PropertySearch = PropertySearch.NoSearch }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should not change state if the cursor is at the end of line.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 12
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteForwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 12
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryPartProperty "name" "pocof"

        [<Fact>]
        let ``should correct state if the cursor is over the query length.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ""
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteForwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ""
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.Required }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should remove the forward selection larger than word.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Select -3
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteForwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.NoSearch }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should remove the forward word larger than selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name pocof a"
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Select -7
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteForwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name "
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.NoSearch }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should remove the backward selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name pocof "
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Select 3
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteForwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":na"
                    InternalState.QueryState.Cursor = 3
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "na" }

            a2.Queries |> testQueryEnd

    module ``with KillBeginningOfLine`` =
        [<Fact>]
        let ``should remove all characters before the specified.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.Cursor = 7 }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardInput

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = "query"
                    InternalState.QueryState.Cursor = 0 }

            a2.Queries |> testQueryPartNormal "query"

        [<Fact>]
        let ``should remove all characters when the cursor is over the query length.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.Cursor = 13 }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardInput

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ""
                    InternalState.QueryState.Cursor = 0 }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should not change state if the cursor is at the begin of line.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "query"
                    InternalState.QueryState.Cursor = 0 }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardInput

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = "query"
                    InternalState.QueryState.Cursor = 0

                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryPartNormal "query"

        [<Fact>]
        let ``should remove all characters before the cursor including the selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    InternalState.QueryState.Cursor = 10 }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardInput

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = "ry"
                    InternalState.QueryState.InputMode = InputMode.Input
                    InternalState.QueryState.Cursor = 0 }

            a2.Queries |> testQueryPartNormal "ry"

        [<Fact>]
        let ``should remove all characters before the cursor and the the selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.InputMode = InputMode.Select -5
                    InternalState.QueryState.Cursor = 5 }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteBackwardInput

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = "ry"
                    InternalState.QueryState.InputMode = InputMode.Input
                    InternalState.QueryState.Cursor = 0 }

            a2.Queries |> testQueryPartNormal "ry"

    module ``with KillEndOfLine`` =
        [<Fact>]
        let ``should remove characters after the current cursor.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.Cursor = 7 }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteForwardInput

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = "example"
                    InternalState.QueryState.Cursor = 7 }

            a2.Queries |> testQueryPartNormal "example"

        [<Fact>]
        let ``should not change state if the cursor is at the end of line.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "example"
                    InternalState.QueryState.Cursor = 7 }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteForwardInput

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = "example"
                    InternalState.QueryState.Cursor = 7
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryPartNormal "example"

        [<Fact>]
        let ``should remove all characters after the cursor including the selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    InternalState.QueryState.Cursor = 7 }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteForwardInput

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = "ex"
                    InternalState.QueryState.InputMode = InputMode.Input
                    InternalState.QueryState.Cursor = 2 }

            a2.Queries |> testQueryPartNormal "ex"

        [<Fact>]
        let ``should remove all characters before the cursor and the the selection.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = "examplequery"
                    InternalState.QueryState.InputMode = InputMode.Select -5
                    InternalState.QueryState.Cursor = 2 }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.DeleteForwardInput

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = "ex"
                    InternalState.QueryState.InputMode = InputMode.Input
                    InternalState.QueryState.Cursor = 2 }

            a2.Queries |> testQueryPartNormal "ex"

    module ``with SelectBackwardChar`` =
        [<Fact>]
        let ``should return QueryState with no change when moving backward on ':name' with Cursor=0.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectBackwardChar

            a1
            |> shouldEqual
                { state with
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with Cursor=4 and InputMode=Select -1 when moving backward on ':name' with Cursor=5 and InputMode=Input.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectBackwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 4
                    InternalState.QueryState.InputMode = InputMode.Select(-1)
                    PropertySearch = PropertySearch.Search "nam" }

            a2.Queries |> testQueryEnd

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

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectBackwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 3
                    InternalState.QueryState.InputMode = InputMode.Select(-2)
                    PropertySearch = PropertySearch.Search "na" }

            a2.Queries |> testQueryEnd

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

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectForwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select 1
                    PropertySearch = PropertySearch.Search "" }

            a2.Queries |> testQueryEnd

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

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectForwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 2
                    InternalState.QueryState.InputMode = InputMode.Select 2
                    PropertySearch = PropertySearch.Search "n" }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with no change when moving forward on ':name' with Cursor=5.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectForwardChar

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name"
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryEnd

    module ``with SelectBackwardWord`` =
        [<Fact>]
        let ``should return QueryState with no change when moving backward with Cursor=0.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectBackwardWord

            a1
            |> shouldEqual
                { state with
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return QueryState with Cursor=1 and InputMode=Select -4 when moving backward with Cursor=5 and InputMode=Input.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectBackwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select(-4)
                    PropertySearch = PropertySearch.Search "" }

            a2.Queries |> testQueryPartProperty "name" "aaa"

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

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectBackwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select(-4)
                    PropertySearch = PropertySearch.Search "" }

            a2.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return QueryState with Cursor=0 and InputMode=Select -4 when moving backward with Cursor=4 and InputMode=Input.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 4
                    PropertySearch = PropertySearch.Search "nam" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectBackwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select(-3)
                    PropertySearch = PropertySearch.Search "" }

            a2.Queries |> testQueryPartProperty "name" "aaa"

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

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectBackwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Cursor = 0
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.NoSearch }

            a2.Queries |> testQueryPartProperty "name" "aaa"

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

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectForwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Select 1
                    PropertySearch = PropertySearch.Search "" }

            a2.Queries |> testQueryPartProperty "name" "aaa"

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

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectForwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Select 6
                    PropertySearch = PropertySearch.NoSearch }

            a2.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return QueryState with Cursor=6 and InputMode=Select 4 when moving forward with Cursor=2 and InputMode=Input.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = PropertySearch.Search "" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectForwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Select 4
                    PropertySearch = PropertySearch.NoSearch }

            a2.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return QueryState with no change when moving forward with Cursor=10.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 10
                    InternalState.QueryState.InputMode = InputMode.Select 10 }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectForwardWord

            a1
            |> shouldEqual
                { state with
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryPartProperty "name" "aaa"

        [<Fact>]
        let ``should return QueryState with Cursor=10 and InputMode=Input when moving forward with Cursor=6 and InputMode=Select 4.``
            ()
            =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name aaa "
                    InternalState.QueryState.Cursor = 6
                    InternalState.QueryState.InputMode = InputMode.Select -4 }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectForwardWord

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Cursor = 10
                    InternalState.QueryState.InputMode = InputMode.Input }

            a2.Queries |> testQueryPartProperty "name" "aaa"

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

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectToBeginningOfLine

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    InternalState.QueryState.InputMode = InputMode.Select -5
                    PropertySearch = PropertySearch.NoSearch }

            a2.Queries |> testQueryEnd

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

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectToBeginningOfLine

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    InternalState.QueryState.InputMode = InputMode.Select -5
                    PropertySearch = PropertySearch.NoSearch }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with no change when moving head on ':name' with Cursor=0.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectToBeginningOfLine

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryEnd

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

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectToEndOfLine

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name" }

            a2.Queries |> testQueryEnd

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

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectToEndOfLine

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name" }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with no change when moving tail on ':name' with Cursor=5.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectToEndOfLine

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name"
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryEnd

    module ``with SelectAll`` =
        [<Fact>]
        let ``should return QueryState with Cursor=5 and InputMode=Select 5 with Cursor=0 and InputMode=Input.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 0
                    PropertySearch = PropertySearch.NoSearch }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectAll

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name" }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with Cursor=5 and InputMode=Select 5 with Cursor=1 and InputMode=Input.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 1
                    InternalState.QueryState.InputMode = InputMode.Input
                    PropertySearch = PropertySearch.Search "" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectAll

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name" }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return QueryState with Cursor=5 and InputMode=Select 5 with Cursor=5.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.SelectAll

            a1
            |> shouldEqual
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    InternalState.QueryState.InputMode = InputMode.Select 5
                    PropertySearch = PropertySearch.Search "name" }

            a2.Queries |> testQueryEnd

    let testStateAndContext action state context expectedState =
        let a1, a2 = invokeAction [] state context action

        a1 |> shouldEqual expectedState

        (a1, a2)

    let testShouldNotChangeQueryContext (expected: QueryContext) (actual: QueryContext) =
        actual.Queries |> shouldEqual expected.Queries
        actual.Operator |> shouldEqual expected.Operator

    module ``with RotateMatcher`` =
        let test before after =
            let stateBefore =
                { state with
                    InternalState.QueryCondition.Matcher = before }

            let context, _ = Query.prepare stateBefore

            let stateAfter =
                { state with
                    InternalState.QueryCondition.Matcher = after }

            testStateAndContext Action.RotateMatcher stateBefore context stateAfter
            |> snd
            |> testShouldNotChangeQueryContext context

        [<Fact>]
        let ``should switch EQ to LIKE.`` () = test Matcher.Eq Matcher.Like

        [<Fact>]
        let ``should switch LIKE to MATCH.`` () = test Matcher.Like Matcher.Match

        [<Fact>]
        let ``should switch MATCh to EQ.`` () = test Matcher.Match Matcher.Eq

    module ``with RotateOperator`` =
        let test before after =
            let stateBefore =
                { state with
                    InternalState.QueryCondition.Operator = before }

            let context, _ = Query.prepare stateBefore

            let stateAfter =
                { state with
                    InternalState.QueryCondition.Operator = after }

            testStateAndContext Action.RotateOperator stateBefore context stateAfter
            |> snd
            |> _.Queries
            |> testQueryEnd

        [<Fact>]
        let ``should switch OR to AND.`` () = test Operator.Or Operator.And

        [<Fact>]
        let ``should switch AND to OR.`` () = test Operator.And Operator.Or

    module ``with ToggleCaseSensitive`` =
        let test before after =
            let stateBefore =
                { state with
                    InternalState.QueryCondition.CaseSensitive = before }

            let context, _ = Query.prepare stateBefore

            let stateAfter =
                { state with
                    InternalState.QueryCondition.CaseSensitive = after }


            testStateAndContext Action.ToggleCaseSensitive stateBefore context stateAfter
            |> snd
            |> testShouldNotChangeQueryContext context

        [<Fact>]
        let ``should return a enabled case sensitive.`` () = test false true

        [<Fact>]
        let ``should return a disabled case sensitive.`` () = test true false

    module ``with ToggleInvertFilter`` =
        let test before after =
            let stateBefore =
                { state with
                    InternalState.QueryCondition.Invert = before }

            let context, _ = Query.prepare stateBefore

            let stateAfter =
                { state with
                    InternalState.QueryCondition.Invert = after }

            testStateAndContext Action.ToggleInvertFilter stateBefore context stateAfter
            |> snd
            |> testShouldNotChangeQueryContext context

        [<Fact>]
        let ``should return a enabled invert filter.`` () = test false true

        [<Fact>]
        let ``should return a disabled invert filter.`` () = test true false

    module ``with ToggleSuppressProperties`` =
        let test before after =
            let stateBefore =
                { state with
                    InternalState.SuppressProperties = before }

            let context, _ = Query.prepare stateBefore

            let stateAfter =
                { state with
                    InternalState.SuppressProperties = after }

            testStateAndContext Action.ToggleSuppressProperties stateBefore context stateAfter
            |> snd
            |> testShouldNotChangeQueryContext context

        [<Fact>]
        let ``should return a enabled suppress property.`` () = test false true

        [<Fact>]
        let ``should return a disabled suppress property.`` () = test true false

    module ``with CompleteProperty`` =
        [<Fact>]
        let ``shouldn't return any difference when a tab is entered with non search mode.`` () =
            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [ "name"; "path" ] state context Action.CompleteProperty

            a1
            |> shouldEqual
                { state with
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``shouldn't return any difference when a tab is entered with empty properties list.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":"
                    PropertySearch = PropertySearch.Search "" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [] state context Action.CompleteProperty

            a1
            |> shouldEqual
                { state with
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``shouldn't return any difference when a tab is entered and found no property completion.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":a"
                    PropertySearch = PropertySearch.Search "a" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [ "name"; "path" ] state context Action.CompleteProperty

            a1
            |> shouldEqual
                { state with
                    Refresh = Refresh.NotRequired }

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return the first completion when a empty keyword.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":"
                    InternalState.QueryState.Cursor = 1
                    PropertySearch = PropertySearch.Search "" }

            let context, _ = Query.prepare state

            let a1, a2 =
                invokeAction [ "first"; "second"; "third" ] state context Action.CompleteProperty

            a1.QueryState.Query |> shouldEqual ":first"
            a1.QueryState.Cursor |> shouldEqual 6

            a1.PropertySearch
            |> function
                | PropertySearch.Rotate(a, b, c) ->
                    a |> shouldEqual ""
                    b |> shouldEqual 0

                    c
                    |> Seq.take 4
                    |> List.ofSeq
                    |> shouldEqual [ "first"; "second"; "third"; "first" ]
                | _ -> failwith "PropertySearch should be Rotate"

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return completion when a property is found.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":p"
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = PropertySearch.Search "p" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [ "Name"; "Path" ] state context Action.CompleteProperty

            a1.QueryState.Query |> shouldEqual ":Path"
            a1.QueryState.Cursor |> shouldEqual 5

            a1.PropertySearch
            |> function
                | PropertySearch.Rotate(a, b, c) ->
                    a |> shouldEqual "p"
                    b |> shouldEqual 0
                    c |> Seq.take 2 |> List.ofSeq |> shouldEqual [ "Path"; "Path" ]
                | _ -> failwith "PropertySearch should be Rotate"

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return completion when some properties are found.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":n"
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = PropertySearch.Search "n" }

            let context, _ = Query.prepare state

            let a1, a2 =
                invokeAction [ "name"; "path"; "number" ] state context Action.CompleteProperty

            a1.QueryState.Query |> shouldEqual ":name"
            a1.QueryState.Cursor |> shouldEqual 5

            a1.PropertySearch
            |> function
                | PropertySearch.Rotate(a, b, c) ->
                    a |> shouldEqual "n"
                    b |> shouldEqual 0
                    c |> Seq.take 3 |> List.ofSeq |> shouldEqual [ "name"; "number"; "name" ]
                | _ -> failwith "PropertySearch should be Rotate"

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should insert completion to mid of query when a property is found.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":n foo"
                    InternalState.QueryState.Cursor = 2
                    PropertySearch = PropertySearch.Search "n" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [ "name"; "path" ] state context Action.CompleteProperty

            a1.QueryState.Query |> shouldEqual ":name foo"
            a1.QueryState.Cursor |> shouldEqual 5

            a1.PropertySearch
            |> function
                | PropertySearch.Rotate(a, b, c) ->
                    a |> shouldEqual "n"
                    b |> shouldEqual 0
                    c |> Seq.take 2 |> List.ofSeq |> shouldEqual [ "name"; "name" ]
                | _ -> failwith "PropertySearch should be Rotate"

            a2.Queries |> testQueryPartProperty "name" "foo"

        [<Fact>]
        let ``shouldn't return any difference when a property is already completed.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [ "name"; "path" ] state context Action.CompleteProperty

            a1.QueryState.Query |> shouldEqual ":name"
            a1.QueryState.Cursor |> shouldEqual 5

            a1.PropertySearch
            |> function
                | PropertySearch.Rotate(a, b, c) ->
                    a |> shouldEqual "name"
                    b |> shouldEqual 0
                    c |> Seq.take 2 |> List.ofSeq |> shouldEqual [ "name"; "name" ]
                | _ -> failwith "PropertySearch should be Rotate"

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``shouldn't return any difference when a property is already completed to mid of query.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name a"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Search "name" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [ "name"; "path" ] state context Action.CompleteProperty

            a1.QueryState.Query |> shouldEqual ":name a"
            a1.QueryState.Cursor |> shouldEqual 5

            a1.PropertySearch
            |> function
                | PropertySearch.Rotate(a, b, c) ->
                    a |> shouldEqual "name"
                    b |> shouldEqual 0
                    c |> Seq.take 2 |> List.ofSeq |> shouldEqual [ "name"; "name" ]
                | _ -> failwith "PropertySearch should be Rotate"

            a2.Queries |> testQueryPartProperty "name" "a"

        [<Fact>]
        let ``should return current completion when a property is already completed and cursor in mid of it.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name a"
                    InternalState.QueryState.Cursor = 4
                    PropertySearch = PropertySearch.Search "nam" }

            let context, _ = Query.prepare state
            let a1, a2 = invokeAction [ "name"; "path" ] state context Action.CompleteProperty

            a1.QueryState.Query |> shouldEqual ":name a"
            a1.QueryState.Cursor |> shouldEqual 5

            a1.PropertySearch
            |> function
                | PropertySearch.Rotate(a, b, c) ->
                    a |> shouldEqual "nam"
                    b |> shouldEqual 0
                    c |> Seq.take 2 |> List.ofSeq |> shouldEqual [ "name"; "name" ]
                | _ -> failwith "PropertySearch should be Rotate"

            a2.Queries |> testQueryPartProperty "name" "a"

        [<Fact>]
        let ``should return next property when rotation.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Rotate("n", 0, Seq.cycle [ "name"; "number" ]) }

            let context, _ = Query.prepare state

            let a1, a2 =
                invokeAction [ "name"; "path"; "number" ] state context Action.CompleteProperty

            a1.QueryState.Query |> shouldEqual ":number"
            a1.QueryState.Cursor |> shouldEqual 7

            a1.PropertySearch
            |> function
                | PropertySearch.Rotate(a, b, c) ->
                    a |> shouldEqual "n"
                    b |> shouldEqual 0
                    c |> Seq.take 3 |> List.ofSeq |> shouldEqual [ "number"; "name"; "number" ]
                | _ -> failwith "PropertySearch should be Rotate"

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return first property when next rotation not found.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":number"
                    InternalState.QueryState.Cursor = 7
                    PropertySearch = PropertySearch.Rotate("n", 0, Seq.cycle [ "number"; "name" ]) }

            let context, _ = Query.prepare state

            let a1, a2 =
                invokeAction [ "name"; "path"; "number" ] state context Action.CompleteProperty

            a1.QueryState.Query |> shouldEqual ":name"
            a1.QueryState.Cursor |> shouldEqual 5

            a1.PropertySearch
            |> function
                | PropertySearch.Rotate(a, b, c) ->
                    a |> shouldEqual "n"
                    b |> shouldEqual 0
                    c |> Seq.take 3 |> List.ofSeq |> shouldEqual [ "name"; "number"; "name" ]
                | _ -> failwith "PropertySearch should be Rotate"

            a2.Queries |> testQueryEnd
