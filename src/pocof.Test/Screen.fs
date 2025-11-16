module PocofTest.Screen

open System
open System.Collections
open System.Management.Automation

open Expecto
open Expecto.Flip

open Pocof.Data
open Pocof.LanguageExtension
open Pocof.Screen
open Pocof.Test

[<Tests>]
let ``tests KeyBatchBuilder`` =
    testList
        "KeyBatchBuilder"
        [

          test "Append accumulates keys and expands buffer (from zero capacity)" {
              let mutable builder = KeyBatchBuilder 0
              let keyA = ConsoleKeyInfo('a', ConsoleKey.A, false, false, false)
              let keyB = ConsoleKeyInfo('b', ConsoleKey.B, false, false, false)

              builder.Append keyA
              builder.Append keyB

              let batch = builder.ToBatch()

              batch.Length |> Expect.equal "should capture both keys" 2
              batch.Buffer[0] |> Expect.equal "should keep first key" keyA
              batch.Buffer[1] |> Expect.equal "should keep second key" keyB
              batch.Buffer.Length > 1 |> Expect.isTrue "should grow capacity"
          }

          test "Append accumulates keys and expands buffer (from initial capacity)" {
              let mutable builder = KeyBatchBuilder 1
              let keyA = ConsoleKeyInfo('a', ConsoleKey.A, false, false, false)
              let keyB = ConsoleKeyInfo('b', ConsoleKey.B, false, false, false)

              builder.Append keyA
              builder.Append keyB

              let batch = builder.ToBatch()

              batch.Length |> Expect.equal "should capture both keys" 2
              batch.Buffer[0] |> Expect.equal "should keep first key" keyA
              batch.Buffer[1] |> Expect.equal "should keep second key" keyB
              batch.Buffer.Length > 1 |> Expect.isTrue "should grow capacity"
          }

          test "Reset clears count but keeps buffer" {
              let mutable builder = KeyBatchBuilder 4
              let key = ConsoleKeyInfo('x', ConsoleKey.X, false, false, false)

              builder.Append key
              let originalBuffer = builder.ToBatch().Buffer

              builder.Reset()

              builder.Count |> Expect.equal "should clear count" 0

              let keyAfter = ConsoleKeyInfo('y', ConsoleKey.Y, false, false, false)
              builder.Append keyAfter
              let bufferAfter = builder.ToBatch().Buffer

              obj.ReferenceEquals(originalBuffer, bufferAfter)
              |> Expect.isTrue "should reuse buffer"
          }


          ]

[<Tests>]
let ``tests Buff writeScreen`` =
    let state: InternalState =
        { QueryState =
            { Query = "foo"
              Cursor = 3
              WindowBeginningCursor = 0
              WindowWidth = 0
              InputMode = InputMode.Input }
          QueryCondition =
            { Matcher = Matcher.Match
              Operator = Operator.And
              CaseSensitive = true
              Invert = false }
          PropertySearch = PropertySearch.NoSearch
          SuppressProperties = false
          Refresh = Refresh.Required }

    let ``query>`` = "query>"
    let ``query>Length`` = ``query>`` |> String.length
    let ``prompt>`` = "prompt>"
    let ``prompt>Length`` = ``prompt>`` |> String.length

    let getRenderedScreen rui state layout prompt =
        // NOTE: avoid cleanup of buff to check screen.
        let buff = new Buff(rui, (fun _ -> Seq.empty), layout, prompt)
        buff.WriteScreen state PSeq.empty <| Ok []
        rui

    testList
        "Buff writeScreen"
        [

          test "When rendering top down" {
              let rui = new MockRawUI()
              let state = state |> InternalState.updateConsoleWidth ``query>Length`` rui.width
              let rui = getRenderedScreen rui state Layout.TopDown ``query>``

              let expected =
                  [ [ "query>foo                                         "
                      "                                    cmatch and [0]" ]
                    generateLine rui.width (rui.height - 2) ]
                  |> List.concat

              rui.screen |> Expect.equal "should render top down" expected
          }

          test "When rendering top down half" {
              let rui = new MockRawUI()

              (rui :> IRawUI).GetCursorPosition()
              |> fun struct (x, y) -> (rui :> IRawUI).SetCursorPosition <| x / 2 <| y / 2 + 1

              let state = state |> InternalState.updateConsoleWidth ``query>Length`` rui.width
              let rui = getRenderedScreen rui state Layout.TopDownHalf ``query>``

              let expected =
                  [ generateLine rui.width (rui.height / 2)
                    [ "query>foo                                         "
                      "                                    cmatch and [0]" ]
                    generateLine rui.width (rui.height / 2 - 2) ]
                  |> List.concat

              rui.screen |> Expect.equal "should render top down half" expected
          }

          test "When rendering bottom up" {
              let rui = new MockRawUI()

              let state =
                  { state with
                      QueryState =
                          { Query = "hello*world*"
                            Cursor = 12
                            WindowBeginningCursor = 0
                            WindowWidth = 0
                            InputMode = InputMode.Input }
                      QueryCondition =
                          { Matcher = Matcher.Like
                            Operator = Operator.Or
                            CaseSensitive = false
                            Invert = true } }
                  |> InternalState.updateConsoleWidth ``prompt>Length`` rui.width

              let rui = getRenderedScreen rui state Layout.BottomUp ``prompt>``

              let expected =
                  [ [ "prompt>hello*world*                               "
                      "                                    notlike or [0]" ]
                    generateLine rui.width (rui.height - 2) ]
                  |> List.concat
                  |> List.rev

              rui.screen |> Expect.equal "should render bottom up" expected
          }

          test "When rendering bottom up half" {
              let rui = new MockRawUI()

              let state: InternalState =
                  { state with
                      InternalState.QueryState.Query = "hello*world*"
                      InternalState.QueryState.Cursor = 12
                      QueryCondition =
                          { Matcher = Matcher.Like
                            Operator = Operator.Or
                            CaseSensitive = false
                            Invert = true } }
                  |> InternalState.updateConsoleWidth ``prompt>Length`` rui.width

              let rui = getRenderedScreen rui state Layout.BottomUpHalf ``prompt>``

              let expected =
                  [ [ "prompt>hello*world*                               "
                      "                                    notlike or [0]" ]
                    generateLine rui.width (rui.height - 2) ]
                  |> List.concat
                  |> List.rev

              rui.screen |> Expect.equal "should render bottom up half" expected
          }

          test "When rendering notification" {
              let rui = new MockRawUI(80, 30)

              let state: InternalState =
                  { state with
                      InternalState.QueryState.Query = @"\"
                      InternalState.QueryState.Cursor = 1
                      InternalState.QueryCondition.CaseSensitive = false }
                  |> InternalState.updateConsoleWidth ``prompt>Length`` rui.width

              use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown, ``prompt>``)
              buff.WriteScreen state PSeq.empty <| Pocof.Query.props [] state

              let expected =
                  List.concat
                      [ [ @"prompt>\                                                                        "
                          @"note>Invalid pattern '\' at offset 1. Illegal \ at end of pattern. match and [0]" ]
                        generateLine rui.width 28 ]

              rui.screen |> Expect.equal "should render notification" expected
          }

          test "When rendering property suggestions" {
              let rui = new MockRawUI(80, 30)
              use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown, ``prompt>``)
              let props = [ 1..20 ] |> Seq.map (fun i -> $"Name%02d{i}")

              let state: InternalState =
                  { state with
                      InternalState.QueryState.Query = @":"
                      InternalState.QueryState.Cursor = 1
                      InternalState.QueryCondition.CaseSensitive = false
                      PropertySearch = PropertySearch.Search("") }
                  |> InternalState.updateConsoleWidth ``prompt>Length`` rui.width

              buff.WriteScreen state PSeq.empty <| (props |> Ok)

              let expected =
                  List.concat
                      [ [ @"prompt>:                                                                        "
                          @"Name01 Name02 Name03 Name04 Name05 Name06 Name07 Name08 Name09 Nam match and [0]" ]
                        generateLine rui.width 28 ]

              rui.screen |> Expect.equal "should render property suggestions" expected
          }

          test "When rendering property suggestions just fit to line" {
              let rui = new MockRawUI(80, 30)
              use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown, ``prompt>``)

              let props =
                  seq {
                      String.replicate 63 "x"
                      "y"
                      "z"
                  }

              let state: InternalState =
                  { state with
                      InternalState.QueryState.Query = @":"
                      InternalState.QueryState.Cursor = 1
                      InternalState.QueryCondition.CaseSensitive = false
                      PropertySearch = PropertySearch.Search("") }
                  |> InternalState.updateConsoleWidth ``prompt>Length`` rui.width

              buff.WriteScreen state PSeq.empty <| (props |> Ok)

              let expected =
                  List.concat
                      [ [ @"prompt>:                                                                        "
                          @"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx y  match and [0]" ]
                        generateLine rui.width 28 ]

              rui.screen |> Expect.equal "render empty property suggestions" expected
          }

          test "When rendering props notification" {
              let rui = new MockRawUI(80, 30)
              use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown, ``prompt>``)

              let state: InternalState =
                  { state with
                      InternalState.QueryState.Query = @":unknown"
                      InternalState.QueryState.Cursor = 8
                      InternalState.QueryCondition.CaseSensitive = false }
                  |> InternalState.updateConsoleWidth ``prompt>Length`` rui.width

              buff.WriteScreen state PSeq.empty <| Error "Property not found"

              let expected =
                  List.concat
                      [ [ @"prompt>:unknown                                                                 "
                          @"note>Property not found                                            match and [0]" ]
                        generateLine rui.width 28 ]

              rui.screen |> Expect.equal "should render props notification" expected
          }

          let formatTableOutString ol =
              PowerShell
                  .Create()
                  .AddCommand("Format-Table")
                  .AddParameter("InputObject", ol)
                  .AddCommand("Out-String")
                  .Invoke()
              |> Seq.map string

          test "When rendering entries under y" {
              let rui = new MockRawUI(60, 30)
              use buff = new Buff(rui, formatTableOutString, Layout.TopDown, ``prompt>``)

              let state: InternalState =
                  { state with
                      InternalState.QueryState.Query = ""
                      InternalState.QueryState.Cursor = 0
                      InternalState.QueryCondition.CaseSensitive = false }
                  |> InternalState.updateConsoleWidth ``prompt>Length`` rui.width

              let entries =
                  [ 1..10 ]
                  |> List.map (fun i -> DictionaryEntry("Number", i) |> Entry.Dict)
                  |> PSeq.ofSeq

              buff.WriteScreen state entries <| Ok []

              let expected =
                  List.concat
                      [ [ @"prompt>                                                     "
                          @"                                              match and [10]"
                          @"                                                            "
                          @"Name                           Value                        "
                          @"----                           -----                        " ]
                        [ 1..10 ]
                        |> List.map (sprintf "Number                         %-2d                           ")
                        generateLine rui.width 15 ]

              rui.screen |> Expect.equal "should render entries under y" expected
          }

          test "When rendering entries over y" {
              let rui = new MockRawUI(60, 30)
              use buff = new Buff(rui, formatTableOutString, Layout.TopDown, ``prompt>``)

              let state: InternalState =
                  { state with
                      InternalState.QueryState.Query = ""
                      InternalState.QueryState.Cursor = 0
                      InternalState.QueryCondition.CaseSensitive = false }
                  |> InternalState.updateConsoleWidth ``prompt>Length`` rui.width

              let entries =
                  [ 1..100 ]
                  |> List.map (fun i -> DictionaryEntry("Number", i) |> Entry.Dict)
                  |> PSeq.ofSeq

              buff.WriteScreen state entries <| Ok []

              let expected =
                  List.concat
                      [ [ @"prompt>                                                     "
                          @"                                             match and [100]"
                          @"                                                            "
                          @"Name                           Value                        "
                          @"----                           -----                        " ]
                        [ 1..25 ]
                        |> List.map (sprintf "Number                         %-2d                           ") ]

              rui.screen |> Expect.equal "should render entries over y" expected
          }

          test "When rendering many wide entries" {
              let rui = new MockRawUI(60, 30)
              use buff = new Buff(rui, formatTableOutString, Layout.TopDown, ``prompt>``)

              let state: InternalState =
                  { state with
                      InternalState.QueryState.Query = ""
                      InternalState.QueryState.Cursor = 0
                      InternalState.QueryCondition.CaseSensitive = false }
                  |> InternalState.updateConsoleWidth ``prompt>Length`` rui.width

              let entries =
                  [ 1..100 ]
                  |> List.map (fun _ -> String.replicate 10 "0123456789" |> PSObject.AsPSObject |> Entry.Obj)
                  |> PSeq.ofSeq

              buff.WriteScreen state entries <| Ok []

              let expected =
                  List.concat
                      [ [ @"prompt>                                                     "
                          @"                                             match and [100]" ]
                        List.replicate 27 (String.replicate 6 "0123456789")
                        [ String(' ', 60) ] ]

              rui.screen
              |> Expect.equal "should render wide entries only within window width" expected
          }

          testList
              "query window"
              [

                let getRenderedScreen query cursor beginning =
                    let rui = new MockRawUI(50, 25)
                    // NOTE: avoid cleanup of buff to check screen.
                    let state =
                        { state with
                            QueryState =
                                { Query = query
                                  Cursor = cursor
                                  WindowBeginningCursor = beginning
                                  WindowWidth = 50 - (``query>`` |> String.length)
                                  InputMode = InputMode.Input }
                            InternalState.QueryCondition.CaseSensitive = false }

                    getRenderedScreen rui state Layout.TopDown "query>"

                test "When rendering head 44 of query when cursor 0" {
                    let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

                    let rui = getRenderedScreen query 0 0

                    let expected =
                        [ [ "query>0-------->1-------->2-------->3-------->4---"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render head 44 of query when cursor 0" expected
                }

                test "When rendering head 44 of query when cursor 44" {
                    let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

                    let rui = getRenderedScreen query 44 0

                    let expected =
                        [ [ "query>0-------->1-------->2-------->3-------->4---"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render head 44 of query when cursor 44" expected
                }

                test "When rendering mid 44 of query when cursor 59" {
                    let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

                    let rui = getRenderedScreen query (44 + 15) 15

                    let expected =
                        [ [ "query>---->2-------->3-------->4-------->5--------"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render mid 44 of query when cursor 59" expected
                }

                test "When rendering tail 44 of query when cursor 100" {
                    let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

                    let rui = getRenderedScreen query 100 56

                    let expected =
                        [ [ "query>--->6-------->7-------->8-------->9-------->"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render tail 44 of query when cursor 100" expected
                }

                let intToChar i =
                    Char.ConvertFromUtf32(i + Char.ConvertToUtf32("０", 0))

                test "When rendering head 40 of query when cursor 0 with full-width characters" {
                    let query =
                        [ 0..9 ] |> List.map (intToChar >> sprintf "%s------->") |> String.concat ""

                    let rui = getRenderedScreen query 0 0

                    let expected =
                        [ [ "query>０------->１------->２------->３------->４--"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render head 40 of query when cursor 0 with full-width characters" expected
                }

                test "When rendering head 40 of query when cursor 39 with full-width characters" {
                    let query =
                        [ 0..9 ] |> List.map (intToChar >> sprintf "%s------->") |> String.concat ""

                    let rui = getRenderedScreen query 39 0

                    let expected =
                        [ [ "query>０------->１------->２------->３------->４--"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render head 40 of query when cursor 39 with full-width characters" expected
                }

                test "When rendering mid 40 of query when cursor 53 with full-width characters" {
                    let query =
                        [ 0..9 ] |> List.map (intToChar >> sprintf "%s------->") |> String.concat ""

                    let rui = getRenderedScreen query 53 13

                    let expected =
                        [ [ "query>---->２------->３------->４------->５-------"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render mid 40 of query when cursor 53 with full-width characters" expected
                }

                test "When rendering tail 40 of query when cursor 90 with full-width characters" {
                    let query =
                        [ 0..9 ] |> List.map (intToChar >> sprintf "%s------->") |> String.concat ""

                    let rui = getRenderedScreen query 90 50

                    let expected =
                        [ [ "query>--->６------->７------->８------->９------->"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render tail 40 of query when cursor 90 with full-width characters" expected
                }

                ]

          testList
              "query selection"
              [

                let getRenderedScreen query cursor beginning inputMode =
                    let rui = new MockRawUI(50, 25)
                    // NOTE: avoid cleanup of buff to check screen.
                    let state =
                        { state with
                            QueryState =
                                { Query = query
                                  Cursor = cursor
                                  WindowBeginningCursor = beginning
                                  WindowWidth = 50 - (``query>`` |> String.length)
                                  InputMode = inputMode }
                            InternalState.QueryCondition.CaseSensitive = false }

                    getRenderedScreen rui state Layout.TopDown "query>"

                test "When rendering query without selection" {
                    let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

                    let rui = getRenderedScreen query 0 0 InputMode.Input

                    let expected =
                        [ [ "query>0-------->1-------->2-------->3-------->4---"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen |> Expect.equal "should render query without selection" expected
                }

                test "When rendering query with cursor at 0 and selection from 0 to 10" {
                    let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

                    let rui = getRenderedScreen query 0 0 <| InputMode.Select(-10)

                    let expected =
                        [ [ $"query>{escapeSequenceInvert}0-------->{escapeSequenceResetInvert}1-------->2-------->3-------->4---"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render query with cursor at 0 and selection from 0 to 10" expected
                }

                test "When rendering query with cursor at 10 and selection from 0 to 10" {
                    let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

                    let rui = getRenderedScreen query 10 0 <| InputMode.Select(10)

                    let expected =
                        [ [ $"query>{escapeSequenceInvert}0-------->{escapeSequenceResetInvert}1-------->2-------->3-------->4---"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render query with cursor at 10 and selection from 0 to 10" expected
                }

                test "When rendering query with cursor at 0 and selection from 0 to 40" {
                    let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

                    let rui = getRenderedScreen query 0 0 <| InputMode.Select(-40)

                    let expected =
                        [ [ $"query>{escapeSequenceInvert}0-------->1-------->2-------->3-------->{escapeSequenceResetInvert}4---"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render query with cursor at 0 and selection from 0 to 40" expected
                }

                test "When rendering query with cursor at 30 and selection from 30 to 40" {
                    let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

                    let rui = getRenderedScreen query 30 30 <| InputMode.Select(-10)

                    let expected =
                        [ [ $"query>{escapeSequenceInvert}3-------->{escapeSequenceResetInvert}4-------->5-------->6-------->7---"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render query with cursor at 30 and selection from 30 to 40" expected
                }

                test "When rendering query with cursor at 40 and selection from 30 to 40" {
                    let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

                    let rui = getRenderedScreen query 40 30 <| InputMode.Select(10)

                    let expected =
                        [ [ $"query>{escapeSequenceInvert}3-------->{escapeSequenceResetInvert}4-------->5-------->6-------->7---"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render query with cursor at 40 and selection from 30 to 40" expected
                }

                test "When rendering query with cursor at 60 and selection from 20 to 60" {
                    let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

                    let rui = getRenderedScreen query 60 30 <| InputMode.Select(40)

                    let expected =
                        [ [ $"query>{escapeSequenceInvert}3-------->4-------->5-------->{escapeSequenceResetInvert}6-------->7---"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render query with cursor at 60 and selection from 20 to 60" expected
                }

                test "When rendering query with cursor at 90 and selection from 90 to 100" {
                    let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

                    let rui = getRenderedScreen query 90 56 <| InputMode.Select(-10)

                    let expected =
                        [ [ $"query>--->6-------->7-------->8-------->{escapeSequenceInvert}9-------->{escapeSequenceResetInvert}"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render query with cursor at 90 and selection from 90 to 100" expected
                }

                test "When rendering query with cursor at 100 and selection from 90 to 100" {
                    let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

                    let rui = getRenderedScreen query 100 56 <| InputMode.Select(10)

                    let expected =
                        [ [ $"query>--->6-------->7-------->8-------->{escapeSequenceInvert}9-------->{escapeSequenceResetInvert}"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render query with cursor at 100 and selection from 90 to 100" expected
                }

                test "When rendering query with cursor at 100 and selection from 50 to 100" {
                    let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

                    let rui = getRenderedScreen query 100 56 <| InputMode.Select(50)

                    let expected =
                        [ [ $"query>{escapeSequenceInvert}--->6-------->7-------->8-------->9-------->{escapeSequenceResetInvert}"
                            "                                     match and [0]" ]
                          generateLine rui.width (rui.height - 2) ]
                        |> List.concat

                    rui.screen
                    |> Expect.equal "should render query with cursor at 100 and selection from 50 to 100" expected
                }

                ]

          ]

[<Tests>]
let tests_getConsoleWidth =
    testList
        "Buff getConsoleWidth"
        [

          test "When getting console width" {
              let rui = new MockRawUI(60, 30)
              use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown, "prompt>")
              buff.GetConsoleWidth() |> Expect.equal "should return console width" 60
          }

          ]

[<Tests>]
let tests_getKey =
    testList
        "Buff getKey"
        [

          test "When getting key" {
              let rui = new MockRawUI()
              use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown, "prompt>")

              buff.GetKey().Buffer[0]
              |> Expect.equal
                  "should return key sequence"
                  (new ConsoleKeyInfo('\000', ConsoleKey.Enter, false, false, false))
          }

          ]
