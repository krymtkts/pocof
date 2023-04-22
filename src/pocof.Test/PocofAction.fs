module PocofAction

open Xunit
open FsUnitTyped
open System
open System.Collections
open pocof

module ``toKeyPattern should returns`` =
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

module ``get should returns`` =
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
            Map [ ({ Modifier = 7; Key = ConsoleKey.E }, PocofData.Finish)
                  ({ Modifier = 0
                     Key = ConsoleKey.Escape },
                   PocofData.Noop) ]

        let getKey = fun () -> new ConsoleKeyInfo('e', ConsoleKey.E, true, true, true)
        let actual = PocofAction.get keyMap getKey

        actual |> shouldEqual PocofData.Finish

    [<Fact>]
    let ``Action if matched.`` () =
        let keyMap =
            ([ ({ Modifier = 7; Key = ConsoleKey.E }: PocofData.KeyPattern)
               { Modifier = 0
                 Key = ConsoleKey.Escape } ],
             [ PocofData.Finish; PocofData.Noop ],
             PocofAction.defaultKeymap)
            |||> List.foldBack2 Map.add

        let actual =
            PocofAction.get keyMap (fun () -> new ConsoleKeyInfo('u', ConsoleKey.U, false, true, false))

        actual
        |> shouldEqual PocofData.KillBeginningOfLine

        let actual =
            PocofAction.get keyMap (fun () -> new ConsoleKeyInfo('e', ConsoleKey.E, true, true, true))

        actual |> shouldEqual PocofData.Finish

        let actual =
            PocofAction.get keyMap (fun () -> new ConsoleKeyInfo('\000', ConsoleKey.Escape, false, true, false))

        actual |> shouldEqual PocofData.Noop

    [<Fact>]
    let ``Action if matched to no modifier key.`` () =
        let getKey = fun () -> new ConsoleKeyInfo('a', ConsoleKey.Home, false, false, false)
        let actual = PocofAction.get PocofAction.defaultKeymap getKey

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
        let actual = PocofAction.get PocofAction.defaultKeymap getKey

        actual |> shouldEqual PocofData.Noop

    [<Fact>]
    let ``None if not match the keymap.`` () =
        let getKey =
            fun () -> new ConsoleKeyInfo('\000', ConsoleKey.F1, false, false, false)

        let actual = PocofAction.get PocofAction.defaultKeymap getKey

        actual |> shouldEqual PocofData.Noop

module ``convertKeymaps should returns`` =
    [<Fact>]
    let ``map transformed from hastable`` () =
        let h = new Hashtable()
        h.Add("control+alt+shift+x", "cancel")
        h.Add("ESCAPE", "NOOP")

        let expected =
            ([ ({ Modifier = 7; Key = ConsoleKey.X }: PocofData.KeyPattern)
               { Modifier = 0
                 Key = ConsoleKey.Escape } ],
             [ PocofData.Cancel; PocofData.Noop ],
             PocofAction.defaultKeymap)
            |||> List.foldBack2 Map.add

        PocofAction.convertKeymaps h
        |> shouldEqual expected

    [<Fact>]
    let ``default map from null hashtable`` () =
        PocofAction.convertKeymaps null
        |> shouldEqual PocofAction.defaultKeymap
