module Tests

open Xunit
open FsUnitTyped
open pocof
open System
open System.Collections


module ``PocofAction Tests`` =
    module ``toKeyPattern shoud returns`` =
        [<Fact>]
        let ``A without modifiers.`` () =
            PocofAction.toKeyPattern "A"
            |> shouldEqual (Ok { Modifier = 0; Key = ConsoleKey.A })

        [<Fact>]
        let ``A with Alt.`` () =
            PocofAction.toKeyPattern "alt+a"
            |> shouldEqual (Ok { Modifier = 1; Key = ConsoleKey.A })

        [<Fact>]
        let ``A with Alt and Shift.`` () =
            PocofAction.toKeyPattern "Alt+Shift+A"
            |> shouldEqual (Ok { Modifier = 3; Key = ConsoleKey.A })

        [<Fact>]
        let ``A with Ctrl, Alt and Shift.`` () =
            PocofAction.toKeyPattern "control+alt+shift+A"
            |> shouldEqual (Ok { Modifier = 7; Key = ConsoleKey.A })

        [<Fact>]
        let ``Error when empty.`` () =
            PocofAction.toKeyPattern ""
            |> shouldEqual (Error "Unsupported key ''.")

        [<Fact>]
        let ``Error when unsupported combination.`` () =
            PocofAction.toKeyPattern "c+c+c"
            |> shouldEqual (Error "Unsupported combination 'c+c+c'.")

    module ``get should returns `` =
        [<Fact>]
        let ``PocofData.AddChar if no modifier is specified.`` () =
            let getKey = fun () -> new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false)
            let actual = PocofAction.get Map.empty getKey

            actual |> shouldEqual (PocofData.AddChar 'a')

        [<Fact>]
        let ``PocofData.AddChar if symbol with shift.`` () =
            let getKey = fun () -> new ConsoleKeyInfo(':', ConsoleKey.Oem1, true, false, false)
            let actual = PocofAction.get Map.empty getKey

            actual |> shouldEqual (PocofData.AddChar ':')

        [<Fact>]
        let ``user-defined Action if matched.`` () =
            let keyMap: Map<PocofData.KeyPattern, PocofData.Action> =
                Map [ ({ Modifier = 7; Key = ConsoleKey.E }, PocofData.Finish) ]

            let getKey = fun () -> new ConsoleKeyInfo('e', ConsoleKey.E, true, true, true)
            let actual = PocofAction.get keyMap getKey

            actual |> shouldEqual PocofData.Finish

        [<Fact>]
        let ``system-defined Action if matched.`` () =
            let getKey = fun () -> new ConsoleKeyInfo('u', ConsoleKey.U, false, true, false)
            let actual = PocofAction.get Map.empty getKey

            actual
            |> shouldEqual PocofData.KillBeginningOfLine

        [<Fact>]
        let ``system-defined Action if matched to no modifier key.`` () =
            let getKey = fun () -> new ConsoleKeyInfo('a', ConsoleKey.Home, false, false, false)
            let actual = PocofAction.get Map.empty getKey

            actual |> shouldEqual PocofData.BeginningOfLine

        [<Fact>]
        let ``PocofData.AddChar if not match the keymap.`` () =
            let keyMap: Map<PocofData.KeyPattern, PocofData.Action> =
                Map [ ({ Modifier = 1; Key = ConsoleKey.U }, PocofData.KillBeginningOfLine) ]

            let getKey = fun () -> new ConsoleKeyInfo('u', ConsoleKey.U, false, true, true)
            let actual = PocofAction.get keyMap getKey

            actual |> shouldEqual (PocofData.AddChar 'u')

        [<Fact>]
        let ``PocofData.None if the control character not match the keymap.`` () =
            let getKey = fun () -> new ConsoleKeyInfo('\009', ConsoleKey.Tab, false, true, true)
            let actual = PocofAction.get Map.empty getKey

            actual |> shouldEqual PocofData.Noop

    // [<Fact>]
    // let ``None if not match the keymap.`` () =
    //     let getKey =
    //         fun () -> new ConsoleKeyInfo('\n', ConsoleKey.Enter, false, false, false)

    //     let actual = PocofAction.get Map.empty getKey

    //     actual |> shouldEqual PocofData.None


    module ``convertKeymaps should returns `` =
        [<Fact>]
        let ``map transformed from hastable`` () =
            let h = new Hashtable()
            h.Add("Control+Alt+Shift+X", "Cancel")

            PocofAction.convertKeymaps h
            |> shouldEqual (Map[({ Modifier = 7; Key = ConsoleKey.X }, PocofData.Cancel)])

module ``PocofData Tests`` =
    open Microsoft.FSharp.Reflection

    module ``Action fromString shoud returns`` =
        [<Fact>]
        let ``Error Unknown.`` () =
            PocofData.Action.fromString "Unknown"
            |> shouldEqual (Error "Unknown case 'Unknown'.")

        [<Fact>]
        let ``Error when AddChar.`` () =
            PocofData.Action.fromString "AddChar"
            |> shouldEqual (Error "Unknown case 'AddChar'.")

        [<Fact>]
        let ``known actions excluding AddChar.`` () =
            FSharpType.GetUnionCases(typeof<PocofData.Action>)
            |> Seq.filter (fun a -> a.Name <> "AddChar")
            |> Seq.iter (fun a ->
                PocofData.Action.fromString a.Name
                |> shouldEqual (Ok(FSharpValue.MakeUnion(a, [||]) :?> PocofData.Action)))

    let ``Error Unknown.``<'a> (fromString: string -> 'a) =
        shouldFail (fun () -> fromString "Unknown" |> ignore)

    let ``known matchers.``<'a> (fromString: string -> 'a) =
        FSharpType.GetUnionCases(typeof<'a>)
        |> Seq.iter (fun (a: UnionCaseInfo) ->
            fromString a.Name
            |> shouldEqual (FSharpValue.MakeUnion(a, [||]) :?> 'a))

    module ``Matcher fromString shoud returns`` =
        [<Fact>]
        let ``Error Unknown.`` () =
            ``Error Unknown.``<PocofData.Matcher> PocofData.Matcher.fromString

        [<Fact>]
        let ``known matchers.`` () =
            ``known matchers.``<PocofData.Matcher> PocofData.Matcher.fromString

    module ``Operator fromString shoud returns`` =
        [<Fact>]
        let ``Error Unknown.`` () =
            ``Error Unknown.``<PocofData.Operator> PocofData.Operator.fromString

        [<Fact>]
        let ``known matchers.`` () =
            ``known matchers.``<PocofData.Operator> PocofData.Operator.fromString

    module ``Layout fromString shoud returns`` =
        [<Fact>]
        let ``Error Unknown.`` () =
            ``Error Unknown.``<PocofData.Layout> PocofData.Layout.fromString

        [<Fact>]
        let ``known matchers.`` () =
            ``known matchers.``<PocofData.Layout> PocofData.Layout.fromString

    module ``QueryState toString should returns`` =
        [<Fact>]
        let ``eq and`` () =
            let actual: PocofData.QueryState =
                { Matcher = PocofData.Matcher.EQ
                  Operator = PocofData.Operator.AND
                  CaseSensitive = false
                  Invert = false }

            actual.toString |> shouldEqual "eq and"

        [<Fact>]
        let ``cne or`` () =
            let actual: PocofData.QueryState =
                { Matcher = PocofData.Matcher.EQ
                  Operator = PocofData.Operator.OR
                  CaseSensitive = true
                  Invert = true }

            actual.toString |> shouldEqual "cne or"

        [<Fact>]
        let ``like and`` () =
            let actual: PocofData.QueryState =
                { Matcher = PocofData.Matcher.LIKE
                  Operator = PocofData.Operator.AND
                  CaseSensitive = false
                  Invert = false }

            actual.toString |> shouldEqual "like and"

        [<Fact>]
        let ``notcmatch or`` () =
            let actual: PocofData.QueryState =
                { Matcher = PocofData.Matcher.MATCH
                  Operator = PocofData.Operator.OR
                  CaseSensitive = true
                  Invert = true }

            actual.toString |> shouldEqual "notcmatch or"

    module ``initConfig `` =
        [<Fact>]
        let ``should returns tuples`` () =
            PocofData.initConfig
                { Query = ":name"
                  Matcher = "like"
                  Operator = "and"
                  CaseSensitive = true
                  InvertQuery = true
                  NotInteractive = true
                  SuppressProperties = true
                  Prompt = "prompt"
                  Layout = "TopDown"
                  Keymaps = Map [ ({ Modifier = 7; Key = ConsoleKey.X }, PocofData.Cancel) ] }
            |> shouldEqual
            <| ({ Prompt = "prompt"
                  Layout = PocofData.Layout.TopDown
                  Keymaps = Map [ ({ Modifier = 7; Key = ConsoleKey.X }, PocofData.Cancel) ]
                  NotInteractive = true },
                { Query = ":name"
                  QueryState =
                    { Matcher = PocofData.Matcher.LIKE
                      Operator = PocofData.Operator.AND
                      CaseSensitive = true
                      Invert = true }
                  PropertySearch = PocofData.PropertySearch.Search "name"
                  Notification = ""
                  SuppressProperties = true },
                { X = 5; Y = 0 })

        [<Fact>]
        let ``should fail due to unknown Matcher.`` () =
            shouldFail (fun () ->
                PocofData.initConfig
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
                PocofData.initConfig
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
                PocofData.initConfig
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

    module ``invokeAction `` =
        let defaultState: PocofData.InternalState =
            { Query = ""
              QueryState =
                { Matcher = PocofData.Matcher.MATCH
                  Operator = PocofData.Operator.OR
                  CaseSensitive = false
                  Invert = false }
              PropertySearch = PocofData.PropertySearch.NonSearch
              Notification = ""
              SuppressProperties = false }

        let defaultPosition: PocofData.Position = { X = 0; Y = 0 }

        module ``with AddChar`` =
            [<Fact>]
            let ``should return a property search state and position.x = 1 when the char is colon.`` () =
                PocofData.invokeAction (PocofData.AddChar ':') defaultState { X = 0; Y = 0 }
                |> shouldEqual
                <| ({ defaultState with
                        Query = ":"
                        PropertySearch = PocofData.PropertySearch.Search "" },
                    { X = 1; Y = 0 })

            [<Fact>]
            let ``should return a non-search state and position.X = 6 when the char is space.`` () =
                PocofData.invokeAction
                    (PocofData.AddChar ' ')
                    { defaultState with
                        Query = ":name"
                        PropertySearch = PocofData.PropertySearch.Search "name" }
                    { X = 5; Y = 0 }
                |> shouldEqual
                <| ({ defaultState with
                        Query = ":name "
                        PropertySearch = PocofData.PropertySearch.NonSearch },
                    { X = 6; Y = 0 })

        module ``with BackwardChar`` =
            [<Fact>]
            let ``should return state with pos unmodified when moving forward on ':name' with position.X=0.`` () =
                let state =
                    { defaultState with
                        Query = ":name"
                        PropertySearch = PocofData.PropertySearch.NonSearch }

                let position: PocofData.Position = { X = 0; Y = 0 }

                PocofData.invokeAction (PocofData.BackwardChar) state position
                |> shouldEqual
                <| (state, position)

            [<Fact>]
            let ``should return state with position.X=4 when moving forward on ':name' with position.X=5.`` () =
                PocofData.invokeAction
                    (PocofData.BackwardChar)
                    { defaultState with
                        Query = ":name"
                        PropertySearch = PocofData.PropertySearch.Search "name" }
                    { X = 5; Y = 0 }
                |> shouldEqual
                <| ({ defaultState with
                        Query = ":name"
                        PropertySearch = PocofData.PropertySearch.Search "nam" },
                    { X = 4; Y = 0 })

        module ``with ForwardChar`` =
            [<Fact>]
            let ``should return state with position.X=2 when moving forward on ':name' with position.X=1.`` () =
                PocofData.invokeAction
                    PocofData.ForwardChar
                    { defaultState with
                        Query = ":name"
                        PropertySearch = PocofData.PropertySearch.Search "" }
                    { X = 1; Y = 0 }
                |> shouldEqual
                <| ({ defaultState with
                        Query = ":name"
                        PropertySearch = PocofData.PropertySearch.Search "n" },
                    { X = 2; Y = 0 })

            [<Fact>]
            let ``should return state with pos unmodified when moving forward on ':name' with position.X=5 and query.Length=3.``
                ()
                =
                PocofData.invokeAction
                    PocofData.ForwardChar
                    { defaultState with
                        Query = ":name"
                        PropertySearch = PocofData.PropertySearch.Search "name" }
                    { X = 5; Y = 0 }
                |> shouldEqual
                <| ({ defaultState with
                        Query = ":name"
                        PropertySearch = PocofData.PropertySearch.Search "name" },
                    { X = 5; Y = 0 })

        module ``with BeginningOfLine`` =
            ()

        module ``with EndOfLine`` =
            ()

        module ``with DeleteBackwardChar`` =
            [<Fact>]
            let ``should remove the character to the left of cursor, making state.Query one character shorter.`` () =
                PocofData.invokeAction
                    PocofData.DeleteBackwardChar
                    { defaultState with
                        Query = ":name "
                        PropertySearch = PocofData.PropertySearch.NonSearch }
                    { X = 6; Y = 0 }
                |> shouldEqual
                <| ({ defaultState with
                        Query = ":name"
                        PropertySearch = PocofData.PropertySearch.Search "name" },
                    { X = 5; Y = 0 })

            [<Fact>]
            let ``should not change state if the cursor position is at the begin of line.`` () =
                PocofData.invokeAction
                    PocofData.DeleteBackwardChar
                    { defaultState with
                        Query = ":name"
                        PropertySearch = PocofData.PropertySearch.NonSearch }
                    { X = 0; Y = 0 }
                |> shouldEqual
                <| ({ defaultState with
                        Query = ":name"
                        PropertySearch = PocofData.PropertySearch.NonSearch },
                    { X = 0; Y = 0 })

        module ``with DeleteForwardChar`` =
            [<Fact>]
            let ``should remove the character to the right of cursor, making state.Query one character shorter.`` () =
                PocofData.invokeAction
                    PocofData.DeleteForwardChar
                    { defaultState with
                        Query = ":name "
                        PropertySearch = PocofData.PropertySearch.Search "name" }
                    { X = 0; Y = 0 }
                |> shouldEqual
                <| ({ defaultState with
                        Query = "name "
                        PropertySearch = PocofData.PropertySearch.NonSearch },
                    { X = 0; Y = 0 })

            [<Fact>]
            let ``should not change state if the cursor position is at the end of line.`` () =
                PocofData.invokeAction
                    PocofData.DeleteForwardChar
                    { defaultState with
                        Query = ":name"
                        PropertySearch = PocofData.PropertySearch.Search "name" }
                    { X = 5; Y = 0 }
                |> shouldEqual
                <| ({ defaultState with
                        Query = ":name"
                        PropertySearch = PocofData.PropertySearch.Search "name" },
                    { X = 5; Y = 0 })

        module ``with KillBeginningOfLine`` =
            [<Fact>]
            let ``should remove all characters before the specified position.`` () =
                PocofData.invokeAction
                    PocofData.KillBeginningOfLine
                    { defaultState with Query = "examplequery" }
                    { X = 7; Y = 0 }
                |> shouldEqual
                <| ({ defaultState with Query = "query" }, { X = 0; Y = 0 })

            [<Fact>]
            let ``should not change state if the cursor position is at the begin of line.`` () =
                PocofData.invokeAction
                    PocofData.KillBeginningOfLine
                    { defaultState with Query = "query" }
                    { X = 0; Y = 0 }
                |> shouldEqual
                <| ({ defaultState with Query = "query" }, { X = 0; Y = 0 })

        module ``with KillEndOfLine`` =
            [<Fact>]
            let ``should remove characters after the current cursor position.`` () =
                PocofData.invokeAction
                    PocofData.KillEndOfLine
                    { defaultState with Query = "examplequery" }
                    { X = 7; Y = 0 }
                |> shouldEqual
                <| ({ defaultState with Query = "example" }, { X = 7; Y = 0 })

            [<Fact>]
            let ``should not change state if the cursor position is at the end of line.`` () =
                PocofData.invokeAction PocofData.KillEndOfLine { defaultState with Query = "example" } { X = 7; Y = 0 }
                |> shouldEqual
                <| ({ defaultState with Query = "example" }, { X = 7; Y = 0 })

        let testWithouPosition action state expected =
            PocofData.invokeAction action state defaultPosition
            |> shouldEqual
            <| (expected, defaultPosition)

        module ``with RotateMatcher`` =
            let test before after =
                testWithouPosition
                    PocofData.RotateMatcher
                    { defaultState with QueryState = { defaultState.QueryState with Matcher = before } }
                    { defaultState with QueryState = { defaultState.QueryState with Matcher = after } }

            [<Fact>]
            let ``should switch EQ to LIKE.`` () =
                test PocofData.Matcher.EQ PocofData.Matcher.LIKE

            [<Fact>]
            let ``should switch LIKE to MATCH.`` () =
                test PocofData.Matcher.LIKE PocofData.Matcher.MATCH

            [<Fact>]
            let ``should switch MATCh to EQ.`` () =
                test PocofData.Matcher.MATCH PocofData.Matcher.EQ

        module ``with RotateOperator`` =
            let test before after =
                testWithouPosition
                    PocofData.RotateOperator
                    { defaultState with QueryState = { defaultState.QueryState with Operator = before } }
                    { defaultState with QueryState = { defaultState.QueryState with Operator = after } }

            [<Fact>]
            let ``should switch NONE to OR.`` () =
                test PocofData.Operator.NONE PocofData.Operator.OR

            [<Fact>]
            let ``should switch OR to AND.`` () =
                test PocofData.Operator.OR PocofData.Operator.AND

            [<Fact>]
            let ``should switch AND to NONE.`` () =
                test PocofData.Operator.AND PocofData.Operator.NONE

        module ``with ToggleCaseSensitive`` =
            let test before after =
                testWithouPosition
                    PocofData.ToggleCaseSensitive
                    { defaultState with QueryState = { defaultState.QueryState with CaseSensitive = before } }
                    { defaultState with QueryState = { defaultState.QueryState with CaseSensitive = after } }

            [<Fact>]
            let ``should return a enabled case sensitive.`` () = test false true

            [<Fact>]
            let ``should return a disabled case sensitive.`` () = test true false

        module ``with ToggleInvertFilter`` =
            let test before after () =
                testWithouPosition
                    PocofData.ToggleInvertFilter
                    { defaultState with QueryState = { defaultState.QueryState with Invert = before } }
                    { defaultState with QueryState = { defaultState.QueryState with Invert = after } }

            [<Fact>]
            let ``should return a enabled invert filter.`` () = test false true

            [<Fact>]
            let ``should return a disabled invert filter.`` () = test true false

        module ``with ToggleSuppressProperties`` =
            let test before after () =
                testWithouPosition
                    PocofData.ToggleSuppressProperties
                    { defaultState with SuppressProperties = before }
                    { defaultState with SuppressProperties = after }

            [<Fact>]
            let ``should return a enabled suppress property.`` () = test false true

            [<Fact>]
            let ``should return a disabled suppress property.`` () = test true false

        let noop action =
            PocofData.invokeAction action defaultState defaultPosition
            |> shouldEqual
            <| (defaultState, defaultPosition)


        module ``with SelectUp`` =
            [<Fact>]
            let ``should return any difference when a uparrow is entered.`` () = noop PocofData.SelectUp

        module ``with SelectDown`` =
            [<Fact>]
            let ``should return any difference when a downarrow is entered.`` () = noop PocofData.SelectDown

        module ``with ScrollPageUp`` =
            [<Fact>]
            let ``should return any difference when a pageup is entered.`` () = noop PocofData.ScrollPageUp

        module ``with ScrollPageDown`` =
            [<Fact>]
            let ``should return any difference when a pagedown is entered.`` () = noop PocofData.ScrollPageDown

        module ``with TabExpansion`` =
            [<Fact>]
            let ``should return any difference when a tab is entered.`` () = noop PocofData.TabExpansion

[<EntryPoint>]
let main argv = 0
