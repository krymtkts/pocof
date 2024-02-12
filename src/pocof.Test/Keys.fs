module PocofTest.Keys

open System
open System.Collections

open Xunit
open FsUnitTyped

open Pocof

module ``toKeyPattern should returns`` =
    [<Fact>]
    let ``A without modifiers.`` () =
        Keys.toKeyPattern "A"
        |> shouldEqual (Ok { Modifier = 0; Key = ConsoleKey.A })

    [<Fact>]
    let ``A with Alt.`` () =
        Keys.toKeyPattern "alt+a"
        |> shouldEqual (Ok { Modifier = 1; Key = ConsoleKey.A })

    [<Fact>]
    let ``A with Alt and Shift.`` () =
        Keys.toKeyPattern "Alt+Shift+A"
        |> shouldEqual (Ok { Modifier = 3; Key = ConsoleKey.A })

    [<Fact>]
    let ``A with Ctrl, Alt and Shift.`` () =
        Keys.toKeyPattern "control+alt+shift+A"
        |> shouldEqual (Ok { Modifier = 7; Key = ConsoleKey.A })

    [<Fact>]
    let ``Error when empty.`` () =
        Keys.toKeyPattern ""
        |> shouldEqual (Error "Unsupported key ''.")

    [<Fact>]
    let ``Error when unsupported combination.`` () =
        Keys.toKeyPattern "c+c+c"
        |> shouldEqual (Error "Unsupported combination 'c+c+c'.")

module ``get should returns`` =
    [<Fact>]
    let ``PocofData.AddQuery if no modifier is specified.`` () =
        let key = [ new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false) ]
        let actual = Keys.get Map.empty key

        actual |> shouldEqual (Data.Action.AddQuery "a")

    [<Fact>]
    let ``PocofData.AddQuery if symbol with shift.`` () =
        let getKey = [ new ConsoleKeyInfo(':', ConsoleKey.Oem1, true, false, false) ]
        let actual = Keys.get Map.empty getKey

        actual |> shouldEqual (Data.Action.AddQuery ":")

    [<Fact>]
    let ``PocofData.AddQuery multiple times.`` () =
        let getKey =
            [ new ConsoleKeyInfo('p', ConsoleKey.P, false, false, false)
              new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false)
              new ConsoleKeyInfo('s', ConsoleKey.S, false, false, false)
              new ConsoleKeyInfo('t', ConsoleKey.T, false, false, false)
              new ConsoleKeyInfo('e', ConsoleKey.E, false, false, false) ]

        let actual = Keys.get Map.empty getKey

        actual
        |> shouldEqual (Data.Action.AddQuery "paste")

    [<Fact>]
    let ``user-defined Action if matched.`` () =
        let keyMap: Map<Data.KeyPattern, Data.Action> =
            Map [ ({ Modifier = 7; Key = ConsoleKey.E }, Data.Action.Finish)
                  ({ Modifier = 0
                     Key = ConsoleKey.Escape },
                   Data.Action.Noop) ]

        let key = [ new ConsoleKeyInfo('e', ConsoleKey.E, true, true, true) ]
        let actual = Keys.get keyMap key

        actual |> shouldEqual Data.Action.Finish

    [<Fact>]
    let ``Action if matched.`` () =
        let keyMap =
            ([ ({ Modifier = 7; Key = ConsoleKey.E }: Data.KeyPattern)
               { Modifier = 0
                 Key = ConsoleKey.Escape } ],
             [ Data.Action.Finish; Data.Action.Noop ],
             Keys.defaultKeymap)
            |||> List.foldBack2 Map.add

        let actual =
            Keys.get keyMap [ new ConsoleKeyInfo('u', ConsoleKey.U, false, true, false) ]

        actual
        |> shouldEqual Data.Action.KillBeginningOfLine

        let actual =
            Keys.get keyMap [ new ConsoleKeyInfo('e', ConsoleKey.E, true, true, true) ]

        actual |> shouldEqual Data.Action.Finish

        let actual =
            Keys.get keyMap [ new ConsoleKeyInfo('\000', ConsoleKey.Escape, false, true, false) ]

        actual |> shouldEqual Data.Action.Noop

    [<Fact>]
    let ``Action if matched to no modifier key.`` () =
        let keu = [ new ConsoleKeyInfo('a', ConsoleKey.Home, false, false, false) ]
        let actual = Keys.get Keys.defaultKeymap keu

        actual |> shouldEqual Data.Action.BeginningOfLine

    [<Fact>]
    let ``PocofData.AddQuery if not match the keymap.`` () =
        let keyMap: Map<Data.KeyPattern, Data.Action> =
            Map [ ({ Modifier = 1; Key = ConsoleKey.U }, Data.Action.KillBeginningOfLine) ]

        let key = [ new ConsoleKeyInfo('u', ConsoleKey.U, false, true, true) ]
        let actual = Keys.get keyMap key

        actual |> shouldEqual (Data.Action.AddQuery "u")

    [<Fact>]
    let ``PocofData.None if the control character not match the keymap.`` () =
        let key = [ new ConsoleKeyInfo('\009', ConsoleKey.Tab, false, true, true) ]
        let actual = Keys.get Keys.defaultKeymap key

        actual |> shouldEqual Data.Action.Noop

    [<Fact>]
    let ``None if not match the keymap.`` () =
        let key = [ new ConsoleKeyInfo('\000', ConsoleKey.F1, false, false, false) ]
        let actual = Keys.get Keys.defaultKeymap key

        actual |> shouldEqual Data.Action.Noop

module ``convertKeymaps should returns`` =
    open System.Management.Automation

    [<Fact>]
    let ``map transformed from hashtable`` () =
        let h = new Hashtable()
        h.Add("control+alt+shift+x", "cancel")
        h.Add("ESCAPE", "NOOP")

        let expected =
            ([ ({ Modifier = 7; Key = ConsoleKey.X }: Data.KeyPattern)
               { Modifier = 0
                 Key = ConsoleKey.Escape } ],
             [ Data.Action.Cancel; Data.Action.Noop ],
             Keys.defaultKeymap)
            |||> List.foldBack2 Map.add
            |> Ok

        Keys.convertKeymaps h |> shouldEqual expected

    [<Fact>]
    let ``error if the hashtable contains invalid key or action.`` () =
        let h = new OrderedHashtable()
        h.Add("contrl+x", "cancel")
        h.Add("alte+a", "Finissh")
        h.Add("control+alt+shift+x", "cancel")
        h.Add("ESCAE", "NOOP")
        h.Add("TAB", "CompleteProperties")

        let expected =
            [ "Unsupported combination 'contrl+x'."
              "Unsupported combination 'alte+a'.Unknown Action 'Finissh'."
              "Unsupported key 'ESCAE'."
              "Unknown Action 'CompleteProperties'." ]
            |> String.concat "\n"
            |> Error

        Keys.convertKeymaps h |> shouldEqual expected

    [<Fact>]
    let ``default map from null hashtable`` () =
        let expected = Keys.defaultKeymap |> Ok

        Keys.convertKeymaps null |> shouldEqual expected
