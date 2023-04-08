module PocofData

open Xunit
open FsUnitTyped
open Microsoft.FSharp.Reflection
open System
open pocof

module ``Action fromString should returns`` =
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

module ``Matcher fromString should returns`` =
    [<Fact>]
    let ``Error Unknown.`` () =
        ``Error Unknown.``<PocofData.Matcher> PocofData.Matcher.fromString

    [<Fact>]
    let ``known matchers.`` () =
        ``known matchers.``<PocofData.Matcher> PocofData.Matcher.fromString

module ``Operator fromString should returns`` =
    [<Fact>]
    let ``Error Unknown.`` () =
        ``Error Unknown.``<PocofData.Operator> PocofData.Operator.fromString

    [<Fact>]
    let ``known matchers.`` () =
        ``known matchers.``<PocofData.Operator> PocofData.Operator.fromString

module ``Layout fromString should returns`` =
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

module initConfig =
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

module invokeAction =
    let state: PocofData.InternalState =
        { Query = ""
          QueryState =
            { Matcher = PocofData.Matcher.MATCH
              Operator = PocofData.Operator.OR
              CaseSensitive = false
              Invert = false }
          PropertySearch = PocofData.PropertySearch.NonSearch
          Notification = ""
          SuppressProperties = false }

    let position: PocofData.Position = { X = 0; Y = 0 }

    module ``with AddChar`` =
        [<Fact>]
        let ``should return a property search state and position.x = 1 when the char is colon.`` () =
            PocofData.invokeAction (PocofData.AddChar ':') state { X = 0; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":"
                    PropertySearch = PocofData.PropertySearch.Search "" },
                { X = 1; Y = 0 })

        [<Fact>]
        let ``should return a non-search state and position.X = 6 when the char is space.`` () =
            PocofData.invokeAction
                (PocofData.AddChar ' ')
                { state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "name" }
                { X = 5; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name "
                    PropertySearch = PocofData.PropertySearch.NonSearch },
                { X = 6; Y = 0 })

    module ``with BackwardChar`` =
        [<Fact>]
        let ``should return state with pos unmodified when moving forward on ':name' with position.X=0.`` () =
            let state =
                { state with
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
                { state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "name" }
                { X = 5; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "nam" },
                { X = 4; Y = 0 })

    module ``with ForwardChar`` =
        [<Fact>]
        let ``should return state with position.X=2 when moving forward on ':name' with position.X=1.`` () =
            PocofData.invokeAction
                PocofData.ForwardChar
                { state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "" }
                { X = 1; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "n" },
                { X = 2; Y = 0 })

        [<Fact>]
        let ``should return state with pos unmodified when moving forward on ':name' with position.X=5 and query.Length=3.``
            ()
            =
            PocofData.invokeAction
                PocofData.ForwardChar
                { state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "name" }
                { X = 5; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "name" },
                { X = 5; Y = 0 })

    module ``with BeginningOfLine`` =
        [<Fact>]
        let ``should return state with position.X = 0.`` () =
            PocofData.invokeAction
                PocofData.BeginningOfLine
                { state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "name" }
                { X = 5; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.NonSearch },
                { X = 0; Y = 0 })

        [<Fact>]
        let ``should return state with pos unmodified`` () =
            PocofData.invokeAction
                PocofData.BeginningOfLine
                { state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "name" }
                { X = 0; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.NonSearch },
                { X = 0; Y = 0 })

    module ``with EndOfLine`` =
        [<Fact>]
        let ``should return state with position.X = query length.`` () =
            PocofData.invokeAction
                PocofData.EndOfLine
                { state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "n" }
                { X = 2; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "name" },
                { X = 5; Y = 0 })

        [<Fact>]
        let ``should return state with pos unmodified`` () =
            PocofData.invokeAction
                PocofData.EndOfLine
                { state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "name" }
                { X = 5; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "name" },
                { X = 5; Y = 0 })

    module ``with DeleteBackwardChar`` =
        [<Fact>]
        let ``should remove the character to the left of cursor, making state.Query one character shorter.`` () =
            PocofData.invokeAction
                PocofData.DeleteBackwardChar
                { state with
                    Query = ":name "
                    PropertySearch = PocofData.PropertySearch.NonSearch }
                { X = 6; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "name" },
                { X = 5; Y = 0 })

        [<Fact>]
        let ``should not change state if the cursor position is at the begin of line.`` () =
            PocofData.invokeAction
                PocofData.DeleteBackwardChar
                { state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.NonSearch }
                { X = 0; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.NonSearch },
                { X = 0; Y = 0 })

    module ``with DeleteForwardChar`` =
        [<Fact>]
        let ``should remove the character to the right of cursor, making state.Query one character shorter.`` () =
            PocofData.invokeAction
                PocofData.DeleteForwardChar
                { state with
                    Query = ":name "
                    PropertySearch = PocofData.PropertySearch.Search "name" }
                { X = 0; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = "name "
                    PropertySearch = PocofData.PropertySearch.NonSearch },
                { X = 0; Y = 0 })

        [<Fact>]
        let ``should not change state if the cursor position is at the end of line.`` () =
            PocofData.invokeAction
                PocofData.DeleteForwardChar
                { state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "name" }
                { X = 5; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = PocofData.PropertySearch.Search "name" },
                { X = 5; Y = 0 })

    module ``with KillBeginningOfLine`` =
        [<Fact>]
        let ``should remove all characters before the specified position.`` () =
            PocofData.invokeAction PocofData.KillBeginningOfLine { state with Query = "examplequery" } { X = 7; Y = 0 }
            |> shouldEqual
            <| ({ state with Query = "query" }, { X = 0; Y = 0 })

        [<Fact>]
        let ``should not change state if the cursor position is at the begin of line.`` () =
            PocofData.invokeAction PocofData.KillBeginningOfLine { state with Query = "query" } { X = 0; Y = 0 }
            |> shouldEqual
            <| ({ state with Query = "query" }, { X = 0; Y = 0 })

    module ``with KillEndOfLine`` =
        [<Fact>]
        let ``should remove characters after the current cursor position.`` () =
            PocofData.invokeAction PocofData.KillEndOfLine { state with Query = "examplequery" } { X = 7; Y = 0 }
            |> shouldEqual
            <| ({ state with Query = "example" }, { X = 7; Y = 0 })

        [<Fact>]
        let ``should not change state if the cursor position is at the end of line.`` () =
            PocofData.invokeAction PocofData.KillEndOfLine { state with Query = "example" } { X = 7; Y = 0 }
            |> shouldEqual
            <| ({ state with Query = "example" }, { X = 7; Y = 0 })

    let testStateOnly action state expected =
        PocofData.invokeAction action state position
        |> shouldEqual
        <| (expected, position)

    module ``with RotateMatcher`` =
        let test before after =
            testStateOnly
                PocofData.RotateMatcher
                { state with QueryState = { state.QueryState with Matcher = before } }
                { state with QueryState = { state.QueryState with Matcher = after } }

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
            testStateOnly
                PocofData.RotateOperator
                { state with QueryState = { state.QueryState with Operator = before } }
                { state with QueryState = { state.QueryState with Operator = after } }

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
            testStateOnly
                PocofData.ToggleCaseSensitive
                { state with QueryState = { state.QueryState with CaseSensitive = before } }
                { state with QueryState = { state.QueryState with CaseSensitive = after } }

        [<Fact>]
        let ``should return a enabled case sensitive.`` () = test false true

        [<Fact>]
        let ``should return a disabled case sensitive.`` () = test true false

    module ``with ToggleInvertFilter`` =
        let test before after () =
            testStateOnly
                PocofData.ToggleInvertFilter
                { state with QueryState = { state.QueryState with Invert = before } }
                { state with QueryState = { state.QueryState with Invert = after } }

        [<Fact>]
        let ``should return a enabled invert filter.`` () = test false true

        [<Fact>]
        let ``should return a disabled invert filter.`` () = test true false

    module ``with ToggleSuppressProperties`` =
        let test before after () =
            testStateOnly
                PocofData.ToggleSuppressProperties
                { state with SuppressProperties = before }
                { state with SuppressProperties = after }

        [<Fact>]
        let ``should return a enabled suppress property.`` () = test false true

        [<Fact>]
        let ``should return a disabled suppress property.`` () = test true false

    let noop action =
        PocofData.invokeAction action state position
        |> shouldEqual
        <| (state, position)


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
