module PocofTest.Handle

open Xunit
open FsUnitTyped

open Expecto
open Expecto.Flip

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
        let s, c = invokeAction [] state context action

        s
        |> Expect.equal
            "should return same state"
            { state with
                Refresh = Refresh.NotRequired }

        c.Operator |> Expect.equal "should return same operator" context.Operator

        c.Queries
        |> Expect.hasLength "should return same queries" context.Queries.Length

    let testQueryEnd =
        function
        | [] -> ()
        | _ -> failwith "invalid query node end."

    let testQueryPartNormal value =
        function
        | [ Query.QueryPart.Normal(x) ] -> x value |> Expect.isTrue "normal query should match"
        | _ -> failwith "invalid query part normal."

    let testQueryPartProperty propertyName value =
        function
        | [ Query.QueryPart.Property(x, y) ] ->
            x |> Expect.equal "property name should equal to" propertyName
            y value |> Expect.isTrue "property query should match"
        | _ -> failwith "invalid query part property."

    [<Tests>]
    let tests_Noop =
        testList
            "Noop"
            [

              test "When Noop" { noop Action.Noop }

              ]

    [<Tests>]
    let tests_Cancel =
        testList
            "Cancel"
            [

              test "When Cancel" {
                  let context, _ = Query.prepare state
                  Expect.throws "should fail" (fun () -> invokeAction [] state context Action.Cancel |> ignore)
              }

              ]

    [<Tests>]
    let tests_AddQuery =
        testList
            "AddQuery"
            [

              test "When the query is colon" {
                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [ "name" ] state context (Action.AddQuery ":")

                  a1
                  |> Expect.equal
                      "should return a property search state and cursor=1"
                      { state with
                          InternalState.QueryState.Query = ":"
                          InternalState.QueryState.Cursor = 1
                          PropertySearch = PropertySearch.Search "" }

                  a2.Queries |> testQueryEnd
              }

              test "When the query is space" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [ "name" ] state context (Action.AddQuery " ")

                  a1
                  |> Expect.equal
                      "should return a non-search state and cursor=6"
                      { state with
                          InternalState.QueryState.Query = ":name "
                          InternalState.QueryState.Cursor = 6
                          PropertySearch = PropertySearch.NoSearch }

                  a2.Queries |> testQueryEnd
              }

              test "When the query is selected" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          InternalState.QueryState.InputMode = InputMode.Select 4
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [ "name" ] state context (Action.AddQuery "l")

                  a1
                  |> Expect.equal
                      "should remove the selection before adding query"
                      { state with
                          InternalState.QueryState.Query = ":l"
                          InternalState.QueryState.Cursor = 2
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.Search "l" }

                  a2.Queries |> testQueryEnd
              }

              ]

    [<Tests>]
    let tests_BackwardChar =
        testList
            "BackwardChar"
            [

              test "When moving forward on ':name' with cursor=0" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.BackwardChar

                  a1
                  |> Expect.equal
                      "should return state with Refresh.NotRequired"
                      { state with
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryEnd
              }

              test "When moving forward on ':name' with cursor=5" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.BackwardChar

                  a1
                  |> Expect.equal
                      "should return state with cursor=4"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 4
                          PropertySearch = PropertySearch.Search "nam" }

                  a2.Queries |> testQueryEnd
              }

              test "When moving forward on ':name' with cursor=5 and InputMode=Select" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          InternalState.QueryState.InputMode = InputMode.Select 5
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.BackwardChar

                  a1
                  |> Expect.equal
                      "should return state with cursor=4 and InputMode=Input"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 4
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.Search "nam" }

                  a2.Queries |> testQueryEnd
              }

              ]

    [<Tests>]
    let tests_ForwardChar =
        testList
            "ForwardChar"
            [

              test "When moving forward on ':name' with cursor=1" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 1
                          PropertySearch = PropertySearch.Search "" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.ForwardChar

                  a1
                  |> Expect.equal
                      "should return state with cursor=2"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 2
                          PropertySearch = PropertySearch.Search "n" }

                  a2.Queries |> testQueryEnd
              }

              test "When moving forward on ':name' with cursor=5 and query.Length=3" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.ForwardChar

                  a1
                  |> Expect.equal
                      "should return state with Refresh.NotRequired"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name"
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryEnd
              }

              test "When moving forward on ':name' with cursor=1 and InputMode=Select" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 1
                          InternalState.QueryState.InputMode = InputMode.Select -2
                          PropertySearch = PropertySearch.Search "" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.ForwardChar

                  a1
                  |> Expect.equal
                      "should return state with cursor=2 and InputMode=Input"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 2
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.Search "n" }

                  a2.Queries |> testQueryEnd
              }

              ]

    [<Tests>]
    let tests_BackwardWord =
        testList
            "BackwardWord"
            [

              test "When moving backward on ':name aaa ' with cursor=0" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.BackwardWord

                  a1
                  |> Expect.equal
                      "should return state with Refresh.NotRequired"
                      { state with
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              test "When moving backward on ':name aaa ' with cursor=6" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 6
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.BackwardWord

                  a1
                  |> Expect.equal
                      "should return state with cursor=1"
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 1
                          PropertySearch = PropertySearch.Search "" }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              test "When moving backward on ':name aaa ' with cursor=6 and InputMode=Select" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 6
                          InternalState.QueryState.InputMode = InputMode.Select 6
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.BackwardWord

                  a1
                  |> Expect.equal
                      "should return state with cursor=1 and InputMode=Input"
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 1
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.Search "" }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              ]

    [<Tests>]
    let tests_ForwardWord =
        testList
            "ForwardWord"
            [

              test "When moving forward on ':name aaa ' with cursor=1" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 1
                          PropertySearch = PropertySearch.Search "" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.ForwardWord

                  a1
                  |> Expect.equal
                      "should return state with cursor=6"
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 6
                          PropertySearch = PropertySearch.NoSearch }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              test "When moving forward on ':name aaa ' with cursor=10" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 10
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.ForwardWord

                  a1
                  |> Expect.equal
                      "should return state with Refresh.NotRequired"
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 10
                          PropertySearch = PropertySearch.NoSearch
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              test "When moving forward on ':name aaa ' with cursor=1 and InputMode=Select" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 1
                          InternalState.QueryState.InputMode = InputMode.Select -2
                          PropertySearch = PropertySearch.Search "" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.ForwardWord

                  a1
                  |> Expect.equal
                      "should return state with cursor=6 and InputMode=Input"
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 6
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.NoSearch }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              ]

    [<Tests>]
    let tests_BeginningOfLine =
        testList
            "BeginningOfLine"
            [

              test "When cursor=5" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.BeginningOfLine

                  a1
                  |> Expect.equal
                      "should return state with cursor=0"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch }

                  a2.Queries |> testQueryEnd
              }

              test "When cursor=0" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.BeginningOfLine

                  a1
                  |> Expect.equal
                      "should return state with Refresh.NotRequired"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryEnd
              }

              test "When InputMode=Select and cursor=5" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          InternalState.QueryState.InputMode = InputMode.Select 5
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.BeginningOfLine

                  a1
                  |> Expect.equal
                      "should return state with cursor=0 and InputMode=Input."
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.NoSearch }

                  a2.Queries |> testQueryEnd
              }

              ]

    [<Tests>]
    let tests_EndOfLine =
        testList
            "EndOfLine"
            [

              test "When cursor=0" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.EndOfLine

                  a1
                  |> Expect.equal
                      "should return state with cursor=query length"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name" }

                  a2.Queries |> testQueryEnd
              }
              test "When cursor=query length" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.EndOfLine

                  a1
                  |> Expect.equal
                      "should return state with Refresh.NotRequired"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name"
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryEnd
              }
              test "When InputMode=Select and cursor=0" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          InternalState.QueryState.InputMode = InputMode.Select 5
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.EndOfLine

                  a1
                  |> Expect.equal
                      "should return state with cursor=query length and InputMode=Input"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.Search "name" }

                  a2.Queries |> testQueryEnd
              }

              ]

    [<Tests>]
    let tests_DeleteBackwardChar =
        testList
            "DeleteBackwardChar"
            [

              test "When cursor=query length" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name "
                          InternalState.QueryState.Cursor = 6
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardChar

                  a1
                  |> Expect.equal
                      "should remove the character to the left of cursor, making state.Query one character shorter"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name" }

                  a2.Queries |> testQueryEnd
              }

              test "When the cursor is at the begin of line" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardChar

                  a1
                  |> Expect.equal
                      "should not change state"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryEnd
              }

              test "When the cursor is over the query length" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ""
                          InternalState.QueryState.Cursor = 2
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardChar

                  a1
                  |> Expect.equal
                      "should correct state"
                      { state with
                          InternalState.QueryState.Query = ""
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch
                          Refresh = Refresh.Required }

                  a2.Queries |> testQueryEnd
              }

              test "When InputMode=Select" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name "
                          InternalState.QueryState.Cursor = 6
                          InternalState.QueryState.InputMode = InputMode.Select 3
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardChar

                  a1
                  |> Expect.equal
                      "should remove the selection"
                      { state with
                          InternalState.QueryState.Query = ":na"
                          InternalState.QueryState.Cursor = 3
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.Search "na" }

                  a2.Queries |> testQueryEnd
              }

              ]

    [<Tests>]
    let tests_DeleteForwardChar =
        testList
            "DeleteForwardChar"
            [

              test "When cursor=0" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name "
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteForwardChar

                  a1
                  |> Expect.equal
                      "should remove the character to the right of cursor, making state.Query one character shorter"
                      { state with
                          InternalState.QueryState.Query = "name "
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch }

                  a2.Queries |> testQueryPartNormal "name"
              }

              test "When cursor=5" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteForwardChar

                  a1
                  |> Expect.equal
                      "should not change state if the cursor is at the end of line"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name"
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryEnd
              }

              test "When cursor=6" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 6
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteForwardChar

                  a1
                  |> Expect.equal
                      "should correct state if the cursor is over the query length"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name"
                          Refresh = Refresh.Required }

                  a2.Queries |> testQueryEnd
              }

              test "When InputMode=Select" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name "
                          InternalState.QueryState.Cursor = 3
                          InternalState.QueryState.InputMode = InputMode.Select -3
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardChar

                  a1
                  |> Expect.equal
                      "should remove the selection"
                      { state with
                          InternalState.QueryState.Query = ":na"
                          InternalState.QueryState.Cursor = 3
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.Search "na" }

                  a2.Queries |> testQueryEnd
              }

              ]

    [<Tests>]
    let tests_DeleteBackwardWord =
        testList
            "DeleteBackwardWord"
            [

              test "When cursor=6" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name pocof "
                          InternalState.QueryState.Cursor = 6
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardWord

                  a1
                  |> Expect.equal
                      "should remove the word to the left of cursor, making state.Query one word shorter"
                      { state with
                          InternalState.QueryState.Query = ":pocof "
                          InternalState.QueryState.Cursor = 1
                          PropertySearch = PropertySearch.Search "" }

                  a2.Queries |> testQueryEnd

              }

              test "When cursor=0" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name pocof "
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardWord

                  a1
                  |> Expect.equal
                      "should not change state if the cursor is at the begin of line"
                      { state with
                          InternalState.QueryState.Query = ":name pocof "
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryPartProperty "name" "pocof"
              }

              test "When cursor is invalid" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ""
                          InternalState.QueryState.Cursor = 2
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardWord

                  a1
                  |> Expect.equal
                      "should correct state if the cursor is over the query length"
                      { state with
                          InternalState.QueryState.Query = ""
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch
                          Refresh = Refresh.Required }

                  a2.Queries |> testQueryEnd
              }

              test "When InputMode=Select and cursor=6" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name pocof "
                          InternalState.QueryState.Cursor = 6
                          InternalState.QueryState.InputMode = InputMode.Select -5
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardWord

                  a1
                  |> Expect.equal
                      "should remove the forward selection"
                      { state with
                          InternalState.QueryState.Query = ": "
                          InternalState.QueryState.Cursor = 1
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.Search "" }

                  a2.Queries |> testQueryEnd
              }

              test "When InputMode=Select with 1 char" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name pocof "
                          InternalState.QueryState.Cursor = 11
                          InternalState.QueryState.InputMode = InputMode.Select 1
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardWord

                  a1
                  |> Expect.equal
                      "should remove the backward word larger than selection"
                      { state with
                          InternalState.QueryState.Query = ":name  "
                          InternalState.QueryState.Cursor = 6
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.NoSearch }

                  a2.Queries |> testQueryEnd
              }

              test "When InputMode=Select with 7 char" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name pocof "
                          InternalState.QueryState.Cursor = 11
                          InternalState.QueryState.InputMode = InputMode.Select 7
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardWord

                  a1
                  |> Expect.equal
                      "should remove the backward selection larger than word"
                      { state with
                          InternalState.QueryState.Query = ":nam "
                          InternalState.QueryState.Cursor = 4
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.Search "nam" }

                  a2.Queries |> testQueryEnd
              }

              ]

    [<Tests>]
    let tests_DeleteForwardWord =
        testList
            "DeleteForwardWord"
            [

              test "When cursor=6" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name pocof "
                          InternalState.QueryState.Cursor = 6
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteForwardWord

                  a1
                  |> Expect.equal
                      "should remove the word to the right of cursor, making state.Query one word shorter"
                      { state with
                          InternalState.QueryState.Query = ":name "
                          InternalState.QueryState.Cursor = 6
                          PropertySearch = PropertySearch.NoSearch }

                  a2.Queries |> testQueryEnd
              }

              test "When the cursor is at the end of line" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name pocof "
                          InternalState.QueryState.Cursor = 12
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteForwardWord

                  a1
                  |> Expect.equal
                      "should not change state"
                      { state with
                          InternalState.QueryState.Query = ":name pocof "
                          InternalState.QueryState.Cursor = 12
                          PropertySearch = PropertySearch.NoSearch
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryPartProperty "name" "pocof"
              }

              test "When the cursor is over the query length" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ""
                          InternalState.QueryState.Cursor = 2
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteForwardWord

                  a1
                  |> Expect.equal
                      "should correct state"
                      { state with
                          InternalState.QueryState.Query = ""
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch
                          Refresh = Refresh.Required }

                  a2.Queries |> testQueryEnd
              }

              test "When InputMode=Select with forward 3 chars" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name pocof "
                          InternalState.QueryState.Cursor = 6
                          InternalState.QueryState.InputMode = InputMode.Select -3
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteForwardWord

                  a1
                  |> Expect.equal
                      "should remove the forward selection larger than word"
                      { state with
                          InternalState.QueryState.Query = ":name "
                          InternalState.QueryState.Cursor = 6
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.NoSearch }

                  a2.Queries |> testQueryEnd
              }

              test "When InputMode=Select with 7 chars" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name pocof a"
                          InternalState.QueryState.Cursor = 6
                          InternalState.QueryState.InputMode = InputMode.Select -7
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteForwardWord

                  a1
                  |> Expect.equal
                      "should remove the forward word larger than selection"
                      { state with
                          InternalState.QueryState.Query = ":name "
                          InternalState.QueryState.Cursor = 6
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.NoSearch }

                  a2.Queries |> testQueryEnd
              }

              test "When InputMode=Select with 3 chars" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name pocof "
                          InternalState.QueryState.Cursor = 6
                          InternalState.QueryState.InputMode = InputMode.Select 3
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteForwardWord

                  a1
                  |> Expect.equal
                      "should remove the backward selection"
                      { state with
                          InternalState.QueryState.Query = ":na"
                          InternalState.QueryState.Cursor = 3
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.Search "na" }

                  a2.Queries |> testQueryEnd
              }

              ]

    [<Tests>]
    let tests_KillBeginningOfLine =
        testList
            "KillBeginningOfLine"
            [

              test "When cursor=7" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = "examplequery"
                          InternalState.QueryState.Cursor = 7 }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardInput

                  a1
                  |> Expect.equal
                      "should remove all characters before the specified"
                      { state with
                          InternalState.QueryState.Query = "query"
                          InternalState.QueryState.Cursor = 0 }

                  a2.Queries |> testQueryPartNormal "query"
              }

              test "When cursor=13" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = "examplequery"
                          InternalState.QueryState.Cursor = 13 }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardInput

                  a1
                  |> Expect.equal
                      "should remove all characters when the cursor is over the query length"
                      { state with
                          InternalState.QueryState.Query = ""
                          InternalState.QueryState.Cursor = 0 }

                  a2.Queries |> testQueryEnd
              }

              test "When the cursor is at the begin of line" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = "query"
                          InternalState.QueryState.Cursor = 0 }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardInput

                  a1
                  |> Expect.equal
                      "should not change state"
                      { state with
                          InternalState.QueryState.Query = "query"
                          InternalState.QueryState.Cursor = 0

                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryPartNormal "query"
              }

              test "When InputMode=Select with 5 chars" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = "examplequery"
                          InternalState.QueryState.InputMode = InputMode.Select 5
                          InternalState.QueryState.Cursor = 10 }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardInput

                  a1
                  |> Expect.equal
                      "should remove all characters before the cursor including the selection"
                      { state with
                          InternalState.QueryState.Query = "ry"
                          InternalState.QueryState.InputMode = InputMode.Input
                          InternalState.QueryState.Cursor = 0 }

                  a2.Queries |> testQueryPartNormal "ry"
              }

              test "When InputMode=Selct with forward 5 chars" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = "examplequery"
                          InternalState.QueryState.InputMode = InputMode.Select -5
                          InternalState.QueryState.Cursor = 5 }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteBackwardInput

                  a1
                  |> Expect.equal
                      "should remove all characters before the cursor and the the selection"
                      { state with
                          InternalState.QueryState.Query = "ry"
                          InternalState.QueryState.InputMode = InputMode.Input
                          InternalState.QueryState.Cursor = 0 }

                  a2.Queries |> testQueryPartNormal "ry"
              }

              ]

    [<Tests>]
    let tests_KillEndOfLine =
        testList
            "KillEndOfLine"
            [

              test "When cursor=7" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = "examplequery"
                          InternalState.QueryState.Cursor = 7 }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteForwardInput

                  a1
                  |> Expect.equal
                      "should remove characters after the current cursor"
                      { state with
                          InternalState.QueryState.Query = "example"
                          InternalState.QueryState.Cursor = 7 }

                  a2.Queries |> testQueryPartNormal "example"
              }

              test "When the cursor is at the end of line" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = "example"
                          InternalState.QueryState.Cursor = 7 }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteForwardInput

                  a1
                  |> Expect.equal
                      "should not change state"
                      { state with
                          InternalState.QueryState.Query = "example"
                          InternalState.QueryState.Cursor = 7
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryPartNormal "example"
              }

              test "When InputMode=Select with 5 chars" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = "examplequery"
                          InternalState.QueryState.InputMode = InputMode.Select 5
                          InternalState.QueryState.Cursor = 7 }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteForwardInput

                  a1
                  |> Expect.equal
                      "should remove all characters after the cursor including the selection"
                      { state with
                          InternalState.QueryState.Query = "ex"
                          InternalState.QueryState.InputMode = InputMode.Input
                          InternalState.QueryState.Cursor = 2 }

                  a2.Queries |> testQueryPartNormal "ex"
              }

              test "When InputMode=Select forward 5 chars" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = "examplequery"
                          InternalState.QueryState.InputMode = InputMode.Select -5
                          InternalState.QueryState.Cursor = 2 }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.DeleteForwardInput

                  a1
                  |> Expect.equal
                      "should remove all characters before the cursor and the the selection"
                      { state with
                          InternalState.QueryState.Query = "ex"
                          InternalState.QueryState.InputMode = InputMode.Input
                          InternalState.QueryState.Cursor = 2 }

                  a2.Queries |> testQueryPartNormal "ex"
              }

              ]

    [<Tests>]
    let tests_SelectBackwardChar =
        testList
            "SelectBackwardChar"
            [

              test "When moving backward on ':name' with Cursor=0" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectBackwardChar

                  a1
                  |> Expect.equal
                      "should return QueryState with no change "
                      { state with
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryEnd
              }

              test "When moving backward on ':name' with Cursor=5 and InputMode=Input" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectBackwardChar

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=4 and InputMode=Select -1 "
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 4
                          InternalState.QueryState.InputMode = InputMode.Select(-1)
                          PropertySearch = PropertySearch.Search "nam" }

                  a2.Queries |> testQueryEnd
              }

              test "When moving backward on ':name' with Cursor=4 and InputMode=Select -1" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 4
                          InternalState.QueryState.InputMode = InputMode.Select(-1)
                          PropertySearch = PropertySearch.Search "nam" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectBackwardChar

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=3 and InputMode=Select -2 "
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 3
                          InternalState.QueryState.InputMode = InputMode.Select(-2)
                          PropertySearch = PropertySearch.Search "na" }

                  a2.Queries |> testQueryEnd
              }

              ]

    [<Tests>]
    let tests_SelectForwardChar =
        testList
            "SelectForwardChar"
            [

              test "When moving forward on ':name' with Cursor=0 and InputMode=Input" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.Search "" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectForwardChar

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=1 and InputMode=Select 1"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 1
                          InternalState.QueryState.InputMode = InputMode.Select 1
                          PropertySearch = PropertySearch.Search "" }

                  a2.Queries |> testQueryEnd
              }

              test "When moving forward on ':name' with Cursor=1 and InputMode=Select 1" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 1
                          InternalState.QueryState.InputMode = InputMode.Select 1
                          PropertySearch = PropertySearch.Search "" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectForwardChar

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=1 and InputMode=Select 2"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 2
                          InternalState.QueryState.InputMode = InputMode.Select 2
                          PropertySearch = PropertySearch.Search "n" }

                  a2.Queries |> testQueryEnd
              }

              test "When moving forward on ':name' with Cursor=5" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          InternalState.QueryState.InputMode = InputMode.Select 5
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectForwardChar

                  a1
                  |> Expect.equal
                      "should return QueryState with no change"
                      { state with
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryEnd
              }

              ]

    [<Tests>]
    let tests_SelectBackwardWord =
        testList
            "SelectBackwardWord"
            [

              test "When moving backward with Cursor=0" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectBackwardWord

                  a1
                  |> Expect.equal
                      "should return QueryState with no change"
                      { state with
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              test "When moving backward with Cursor=5 and InputMode=Input" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectBackwardWord

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=1 and InputMode=Select -4"
                      { state with
                          InternalState.QueryState.Cursor = 1
                          InternalState.QueryState.InputMode = InputMode.Select(-4)
                          PropertySearch = PropertySearch.Search "" }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              test "When moving backward with Cursor=4 and InputMode=Select -1" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 4
                          InternalState.QueryState.InputMode = InputMode.Select(-1)
                          PropertySearch = PropertySearch.Search "nam" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectBackwardWord

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=1 and InputMode=Select -4"
                      { state with
                          InternalState.QueryState.Cursor = 1
                          InternalState.QueryState.InputMode = InputMode.Select(-4)
                          PropertySearch = PropertySearch.Search "" }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              test "When moving backward with Cursor=4 and InputMode=Input" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 4
                          PropertySearch = PropertySearch.Search "nam" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectBackwardWord

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=1 and InputMode=Select -3"
                      { state with
                          InternalState.QueryState.Cursor = 1
                          InternalState.QueryState.InputMode = InputMode.Select(-3)
                          PropertySearch = PropertySearch.Search "" }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              test "When moving backward with Cursor=1 and InputMode=Select 1" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 1
                          InternalState.QueryState.InputMode = InputMode.Select 1
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectBackwardWord

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=0 and InputMode=Input"
                      { state with
                          InternalState.QueryState.Cursor = 0
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.NoSearch }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              ]

    [<Tests>]
    let tests_SelectForwardWord =
        testList
            "SelectForwardWord"
            [

              test "When moving forward with Cursor=0 and InputMode=Input" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectForwardWord

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=1 and InputMode=Select 1 "
                      { state with
                          InternalState.QueryState.Cursor = 1
                          InternalState.QueryState.InputMode = InputMode.Select 1
                          PropertySearch = PropertySearch.Search "" }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              test "When moving forward with Cursor=1 and InputMode=Select 1" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 1
                          InternalState.QueryState.InputMode = InputMode.Select 1
                          PropertySearch = PropertySearch.Search "" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectForwardWord

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=6 and InputMode=Select 6"
                      { state with
                          InternalState.QueryState.Cursor = 6
                          InternalState.QueryState.InputMode = InputMode.Select 6
                          PropertySearch = PropertySearch.NoSearch }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              test "When moving forward with Cursor=2 and InputMode=Input" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 2
                          PropertySearch = PropertySearch.Search "" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectForwardWord

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=6 and InputMode=Select 4"
                      { state with
                          InternalState.QueryState.Cursor = 6
                          InternalState.QueryState.InputMode = InputMode.Select 4
                          PropertySearch = PropertySearch.NoSearch }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              test "When moving forward with Cursor=10." {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 10
                          InternalState.QueryState.InputMode = InputMode.Select 10 }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectForwardWord

                  a1
                  |> Expect.equal
                      "should return QueryState with no change"
                      { state with
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              test "When moving forward with Cursor=6 and InputMode=Select 4" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name aaa "
                          InternalState.QueryState.Cursor = 6
                          InternalState.QueryState.InputMode = InputMode.Select -4 }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectForwardWord

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=10 and InputMode=Input"
                      { state with
                          InternalState.QueryState.Cursor = 10
                          InternalState.QueryState.InputMode = InputMode.Input }

                  a2.Queries |> testQueryPartProperty "name" "aaa"
              }

              ]

    [<Tests>]
    let tests_SelectToBeginningOfLine =
        testList
            "SelectToBeginningOfLine"
            [

              test "When moving head on ':name' with Cursor=5 and InputMode=Input" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectToBeginningOfLine

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=0 and InputMode=Select -5"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          InternalState.QueryState.InputMode = InputMode.Select -5
                          PropertySearch = PropertySearch.NoSearch }

                  a2.Queries |> testQueryEnd
              }

              test "When moving head on ':name' with Cursor=4 and InputMode=Select -1" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 4
                          InternalState.QueryState.InputMode = InputMode.Select -1
                          PropertySearch = PropertySearch.Search "nam" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectToBeginningOfLine

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=0 and InputMode=Select -5"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          InternalState.QueryState.InputMode = InputMode.Select -5
                          PropertySearch = PropertySearch.NoSearch }

                  a2.Queries |> testQueryEnd
              }

              test "When moving head on ':name' with Cursor=0" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectToBeginningOfLine

                  a1
                  |> Expect.equal
                      "should return QueryState with no change"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryEnd
              }

              ]

    [<Tests>]
    let tests_SelectToEndOfLine =
        testList
            "SelectToEndOfLine"
            [

              test "When moving tail on ':name' with Cursor=0 and InputMode=Input" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectToEndOfLine

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=5 and InputMode=Select 5"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          InternalState.QueryState.InputMode = InputMode.Select 5
                          PropertySearch = PropertySearch.Search "name" }

                  a2.Queries |> testQueryEnd
              }

              test "When moving tail on ':name' with Cursor=1 and InputMode=Select 1" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 1
                          InternalState.QueryState.InputMode = InputMode.Select 1
                          PropertySearch = PropertySearch.Search "" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectToEndOfLine

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=5 and InputMode=Select 5"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          InternalState.QueryState.InputMode = InputMode.Select 5
                          PropertySearch = PropertySearch.Search "name" }

                  a2.Queries |> testQueryEnd
              }

              test "When moving tail on ':name' with Cursor=5" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectToEndOfLine

                  a1
                  |> Expect.equal
                      "should return QueryState with no change"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name"
                          Refresh = Refresh.NotRequired }

                  a2.Queries |> testQueryEnd
              }

              ]


    [<Tests>]
    let tests_SelectAll =
        testList
            "SelectAll"
            [

              test "When Cursor=0 and InputMode=Input" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 0
                          PropertySearch = PropertySearch.NoSearch }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectAll

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=5 and InputMode=Select 5"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          InternalState.QueryState.InputMode = InputMode.Select 5
                          PropertySearch = PropertySearch.Search "name" }

                  a2.Queries |> testQueryEnd
              }

              test "When Cursor=1 and InputMode=Input" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 1
                          InternalState.QueryState.InputMode = InputMode.Input
                          PropertySearch = PropertySearch.Search "" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectAll

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=5 and InputMode=Select 5"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          InternalState.QueryState.InputMode = InputMode.Select 5
                          PropertySearch = PropertySearch.Search "name" }

                  a2.Queries |> testQueryEnd
              }

              test "When Cursor=5" {
                  let state =
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          PropertySearch = PropertySearch.Search "name" }

                  let context, _ = Query.prepare state
                  let a1, a2 = invokeAction [] state context Action.SelectAll

                  a1
                  |> Expect.equal
                      "should return QueryState with Cursor=5 and InputMode=Select 5"
                      { state with
                          InternalState.QueryState.Query = ":name"
                          InternalState.QueryState.Cursor = 5
                          InternalState.QueryState.InputMode = InputMode.Select 5
                          PropertySearch = PropertySearch.Search "name" }

                  a2.Queries |> testQueryEnd
              }

              ]

    let testStateAndContext action state context expectedState =
        let a1, a2 = invokeAction [] state context action

        a1 |> Expect.equal "" expectedState

        (a1, a2)

    let testShouldNotChangeQueryContext (expected: QueryContext) (actual: QueryContext) =
        actual.Operator |> Expect.equal "operator" expected.Operator
        actual.Queries |> Expect.hasLength "queries length" expected.Queries.Length

        List.zip actual.Queries expected.Queries
        |> Expect.all "" (function
            // TODO: requires better solution.
            | Query.QueryPart.Normal _, Query.QueryPart.Normal _ -> true
            | Query.QueryPart.Property(ap, _), Query.QueryPart.Property(ep, _) -> ap = ep
            | _ -> false)

    [<Tests>]
    let tests_RotateMatcher =
        let testMatcher before after =
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

        testList
            "RotateMatcher"
            [

              test "should switch EQ to LIKE" { testMatcher Matcher.Eq Matcher.Like }
              test "should switch LIKE to MATCH" { testMatcher Matcher.Like Matcher.Match }
              test "should switch MATCh to EQ" { testMatcher Matcher.Match Matcher.Eq }

              ]

    [<Tests>]
    let tests_RotateOperator =
        let testOperator before after =
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

        testList
            "RotateOperator"
            [

              test "should switch OR to AND" { testOperator Operator.Or Operator.And }
              test "should switch AND to OR" { testOperator Operator.And Operator.Or }

              ]

    [<Tests>]
    let tests_ToggleCaseSensitive =
        let testCaseSensitive before after =
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

        testList
            "ToggleCaseSensitive"
            [

              test "should return a enabled case sensitive" { testCaseSensitive false true }
              test "should return a disabled case sensitive" { testCaseSensitive true false }

              ]

    [<Tests>]
    let tests_ToggleInvertFilter =
        let testInvertFilter before after =
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

        testList
            "ToggleInvertFilter"
            [

              test "should return a enabled invert filter" { testInvertFilter false true }
              test "should return a disabled invert filter" { testInvertFilter true false } ]

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
                | PropertySearch.Rotate(a, b) ->
                    a |> shouldEqual ""

                    b
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
                | PropertySearch.Rotate(a, b) ->
                    a |> shouldEqual "p"
                    b |> Seq.take 2 |> List.ofSeq |> shouldEqual [ "Path"; "Path" ]
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
                | PropertySearch.Rotate(a, b) ->
                    a |> shouldEqual "n"
                    b |> Seq.take 3 |> List.ofSeq |> shouldEqual [ "name"; "number"; "name" ]
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
                | PropertySearch.Rotate(a, b) ->
                    a |> shouldEqual "n"
                    b |> Seq.take 2 |> List.ofSeq |> shouldEqual [ "name"; "name" ]
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
                | PropertySearch.Rotate(a, b) ->
                    a |> shouldEqual "name"
                    b |> Seq.take 2 |> List.ofSeq |> shouldEqual [ "name"; "name" ]
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
                | PropertySearch.Rotate(a, b) ->
                    a |> shouldEqual "name"
                    b |> Seq.take 2 |> List.ofSeq |> shouldEqual [ "name"; "name" ]
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
                | PropertySearch.Rotate(a, b) ->
                    a |> shouldEqual "nam"
                    b |> Seq.take 2 |> List.ofSeq |> shouldEqual [ "name"; "name" ]
                | _ -> failwith "PropertySearch should be Rotate"

            a2.Queries |> testQueryPartProperty "name" "a"

        [<Fact>]
        let ``should return next property when rotation.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":name"
                    InternalState.QueryState.Cursor = 5
                    PropertySearch = PropertySearch.Rotate("n", Seq.cycle [ "name"; "number" ]) }

            let context, _ = Query.prepare state

            let a1, a2 =
                invokeAction [ "name"; "path"; "number" ] state context Action.CompleteProperty

            a1.QueryState.Query |> shouldEqual ":number"
            a1.QueryState.Cursor |> shouldEqual 7

            a1.PropertySearch
            |> function
                | PropertySearch.Rotate(a, b) ->
                    a |> shouldEqual "n"
                    b |> Seq.take 3 |> List.ofSeq |> shouldEqual [ "number"; "name"; "number" ]
                | _ -> failwith "PropertySearch should be Rotate"

            a2.Queries |> testQueryEnd

        [<Fact>]
        let ``should return first property when next rotation not found.`` () =
            let state =
                { state with
                    InternalState.QueryState.Query = ":number"
                    InternalState.QueryState.Cursor = 7
                    PropertySearch = PropertySearch.Rotate("n", Seq.cycle [ "number"; "name" ]) }

            let context, _ = Query.prepare state

            let a1, a2 =
                invokeAction [ "name"; "path"; "number" ] state context Action.CompleteProperty

            a1.QueryState.Query |> shouldEqual ":name"
            a1.QueryState.Cursor |> shouldEqual 5

            a1.PropertySearch
            |> function
                | PropertySearch.Rotate(a, b) ->
                    a |> shouldEqual "n"
                    b |> Seq.take 3 |> List.ofSeq |> shouldEqual [ "name"; "number"; "name" ]
                | _ -> failwith "PropertySearch should be Rotate"

            a2.Queries |> testQueryEnd
