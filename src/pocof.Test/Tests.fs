module Tests

open Xunit
open FsUnitTyped
open pocof
open System
open System.Collections


module ``PocofAction Tests`` =
    type ``toKeyPattern shoud returns``() =
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

    type ``get should returns ``() =
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

            actual |> shouldEqual PocofData.None

    // [<Fact>]
    // let ``None if not match the keymap.`` () =
    //     let getKey =
    //         fun () -> new ConsoleKeyInfo('\n', ConsoleKey.Enter, false, false, false)

    //     let actual = PocofAction.get Map.empty getKey

    //     actual |> shouldEqual PocofData.None


    type convertKeymaps() =
        [<Fact>]
        let ``a`` () =
            let h = new Hashtable()
            h.Add("Control+Alt+Shift+X", "Cancel")

            PocofAction.convertKeymaps h
            |> shouldEqual (Map[({ Modifier = 7; Key = ConsoleKey.X }, PocofData.Cancel)])


[<EntryPoint>]
let main argv = 0
