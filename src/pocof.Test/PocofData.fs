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
        Action.fromString "Unknown"
        |> shouldEqual (Error "Unknown case 'Unknown'.")

    [<Fact>]
    let ``Error when AddChar.`` () =
        Action.fromString "AddChar"
        |> shouldEqual (Error "Unknown case 'AddChar'.")

    [<Fact>]
    let ``known actions excluding AddChar.`` () =
        FSharpType.GetUnionCases(typeof<Action>)
        |> Seq.filter (fun a -> a.Name <> "AddChar")
        |> Seq.iter (fun a ->
            Action.fromString a.Name
            |> shouldEqual (Ok(FSharpValue.MakeUnion(a, [||]) :?> Action)))

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
        |> shouldEqual
        <| ({ Prompt = "prompt"
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
            { X = 5; Y = 0 })

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
          PropertySearch = NonSearch
          Notification = ""
          SuppressProperties = false }

    let position: Position = { X = 0; Y = 0 }

    module ``with AddChar`` =
        [<Fact>]
        let ``should return a property search state and position.x = 1 when the char is colon.`` () =
            invokeAction (AddChar ':') state { X = 0; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":"
                    PropertySearch = Search "" },
                { X = 1; Y = 0 })

        [<Fact>]
        let ``should return a non-search state and position.X = 6 when the char is space.`` () =
            invokeAction
                (AddChar ' ')
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }
                { X = 5; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name "
                    PropertySearch = NonSearch },
                { X = 6; Y = 0 })

    module ``with BackwardChar`` =
        [<Fact>]
        let ``should return state with pos unmodified when moving forward on ':name' with position.X=0.`` () =
            let state =
                { state with
                    Query = ":name"
                    PropertySearch = NonSearch }

            let position: Position = { X = 0; Y = 0 }

            invokeAction (BackwardChar) state position
            |> shouldEqual
            <| (state, position)

        [<Fact>]
        let ``should return state with position.X=4 when moving forward on ':name' with position.X=5.`` () =
            invokeAction
                (BackwardChar)
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }
                { X = 5; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = Search "nam" },
                { X = 4; Y = 0 })

    module ``with ForwardChar`` =
        [<Fact>]
        let ``should return state with position.X=2 when moving forward on ':name' with position.X=1.`` () =
            invokeAction
                ForwardChar
                { state with
                    Query = ":name"
                    PropertySearch = Search "" }
                { X = 1; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = Search "n" },
                { X = 2; Y = 0 })

        [<Fact>]
        let ``should return state with pos unmodified when moving forward on ':name' with position.X=5 and query.Length=3.``
            ()
            =
            invokeAction
                ForwardChar
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }
                { X = 5; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = Search "name" },
                { X = 5; Y = 0 })

    module ``with BeginningOfLine`` =
        [<Fact>]
        let ``should return state with position.X = 0.`` () =
            invokeAction
                BeginningOfLine
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }
                { X = 5; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = NonSearch },
                { X = 0; Y = 0 })

        [<Fact>]
        let ``should return state with pos unmodified`` () =
            invokeAction
                BeginningOfLine
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }
                { X = 0; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = NonSearch },
                { X = 0; Y = 0 })

    module ``with EndOfLine`` =
        [<Fact>]
        let ``should return state with position.X = query length.`` () =
            invokeAction
                EndOfLine
                { state with
                    Query = ":name"
                    PropertySearch = Search "n" }
                { X = 2; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = Search "name" },
                { X = 5; Y = 0 })

        [<Fact>]
        let ``should return state with pos unmodified`` () =
            invokeAction
                EndOfLine
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }
                { X = 5; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = Search "name" },
                { X = 5; Y = 0 })

    module ``with DeleteBackwardChar`` =
        [<Fact>]
        let ``should remove the character to the left of cursor, making state.Query one character shorter.`` () =
            invokeAction
                DeleteBackwardChar
                { state with
                    Query = ":name "
                    PropertySearch = NonSearch }
                { X = 6; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = Search "name" },
                { X = 5; Y = 0 })

        [<Fact>]
        let ``should not change state if the cursor position is at the begin of line.`` () =
            invokeAction
                DeleteBackwardChar
                { state with
                    Query = ":name"
                    PropertySearch = NonSearch }
                { X = 0; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = NonSearch },
                { X = 0; Y = 0 })

    module ``with DeleteForwardChar`` =
        [<Fact>]
        let ``should remove the character to the right of cursor, making state.Query one character shorter.`` () =
            invokeAction
                DeleteForwardChar
                { state with
                    Query = ":name "
                    PropertySearch = Search "name" }
                { X = 0; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = "name "
                    PropertySearch = NonSearch },
                { X = 0; Y = 0 })

        [<Fact>]
        let ``should not change state if the cursor position is at the end of line.`` () =
            invokeAction
                DeleteForwardChar
                { state with
                    Query = ":name"
                    PropertySearch = Search "name" }
                { X = 5; Y = 0 }
            |> shouldEqual
            <| ({ state with
                    Query = ":name"
                    PropertySearch = Search "name" },
                { X = 5; Y = 0 })

    module ``with KillBeginningOfLine`` =
        [<Fact>]
        let ``should remove all characters before the specified position.`` () =
            invokeAction KillBeginningOfLine { state with Query = "examplequery" } { X = 7; Y = 0 }
            |> shouldEqual
            <| ({ state with Query = "query" }, { X = 0; Y = 0 })

        [<Fact>]
        let ``should not change state if the cursor position is at the begin of line.`` () =
            invokeAction KillBeginningOfLine { state with Query = "query" } { X = 0; Y = 0 }
            |> shouldEqual
            <| ({ state with Query = "query" }, { X = 0; Y = 0 })

    module ``with KillEndOfLine`` =
        [<Fact>]
        let ``should remove characters after the current cursor position.`` () =
            invokeAction KillEndOfLine { state with Query = "examplequery" } { X = 7; Y = 0 }
            |> shouldEqual
            <| ({ state with Query = "example" }, { X = 7; Y = 0 })

        [<Fact>]
        let ``should not change state if the cursor position is at the end of line.`` () =
            invokeAction KillEndOfLine { state with Query = "example" } { X = 7; Y = 0 }
            |> shouldEqual
            <| ({ state with Query = "example" }, { X = 7; Y = 0 })

    let testStateOnly action state expected =
        invokeAction action state position |> shouldEqual
        <| (expected, position)

    module ``with RotateMatcher`` =
        let test before after =
            testStateOnly
                RotateMatcher
                { state with QueryState = { state.QueryState with Matcher = before } }
                { state with QueryState = { state.QueryState with Matcher = after } }

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
                { state with QueryState = { state.QueryState with Operator = before } }
                { state with QueryState = { state.QueryState with Operator = after } }

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
                { state with QueryState = { state.QueryState with CaseSensitive = before } }
                { state with QueryState = { state.QueryState with CaseSensitive = after } }

        [<Fact>]
        let ``should return a enabled case sensitive.`` () = test false true

        [<Fact>]
        let ``should return a disabled case sensitive.`` () = test true false

    module ``with ToggleInvertFilter`` =
        let test before after () =
            testStateOnly
                ToggleInvertFilter
                { state with QueryState = { state.QueryState with Invert = before } }
                { state with QueryState = { state.QueryState with Invert = after } }

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
        invokeAction action state position |> shouldEqual
        <| (state, position)


    module ``with SelectUp`` =
        [<Fact>]
        let ``should return any difference when a uparrow is entered.`` () = noop SelectUp

    module ``with SelectDown`` =
        [<Fact>]
        let ``should return any difference when a downarrow is entered.`` () = noop SelectDown

    module ``with ScrollPageUp`` =
        [<Fact>]
        let ``should return any difference when a pageup is entered.`` () = noop ScrollPageUp

    module ``with ScrollPageDown`` =
        [<Fact>]
        let ``should return any difference when a pagedown is entered.`` () = noop ScrollPageDown

    module ``with TabExpansion`` =
        [<Fact>]
        let ``should return any difference when a tab is entered.`` () = noop TabExpansion
