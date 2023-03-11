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
        let ``None if not match the keymap.`` () =
            let keyMap: Map<PocofData.KeyPattern, PocofData.Action> =
                Map [ ({ Modifier = 1; Key = ConsoleKey.U }, PocofData.KillBeginningOfLine) ]

            let getKey = fun () -> new ConsoleKeyInfo('u', ConsoleKey.U, false, true, true)
            let actual = PocofAction.get keyMap getKey

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

[<EntryPoint>]
let main argv = 0
