module Tests

open Xunit
open FsUnitTyped
open pocof
open System


module ``PocofAction Tests`` =

    [<Fact>]
    let ``PocofAction.get should returns PocofData.AddChar if no modifier is specified.`` () =
        let getKey = fun () -> new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false)
        let actual = PocofAction.get Map.empty getKey

        actual |> shouldEqual <| PocofData.AddChar 'a'

    [<Fact>]
    let ``PocofAction.get should returns user-defined Action if matched.`` () =
        // TODO: Unfortunately, the modifiers must be defined according to the order of ConsoleModifiers.
        let keyMap = Map [ ("Alt+Shift+Control+E", PocofData.Finish) ]
        let getKey = fun () -> new ConsoleKeyInfo('e', ConsoleKey.E, true, true, true)
        let actual = PocofAction.get keyMap getKey

        actual |> shouldEqual <| PocofData.Finish

    [<Fact>]
    let ``PocofAction.get should returns system-defined Action if matched.`` () =
        let getKey = fun () -> new ConsoleKeyInfo('u', ConsoleKey.U, false, true, false)
        let actual = PocofAction.get Map.empty getKey

        actual |> shouldEqual
        <| PocofData.KillBeginningOfLine

    [<Fact>]
    let ``PocofAction.get should returns system-defined Action if matched to no modifier key.`` () =
        let getKey = fun () -> new ConsoleKeyInfo('a', ConsoleKey.Home, false, false, false)
        let actual = PocofAction.get Map.empty getKey

        actual |> shouldEqual <| PocofData.BeginningOfLine

    [<Fact>]
    let ``PocofAction.get should returns None if not match the keymap.`` () =
        let keyMap = Map [ ("Alt+U", PocofData.KillBeginningOfLine) ]
        let getKey = fun () -> new ConsoleKeyInfo('u', ConsoleKey.U, false, true, true)
        let actual = PocofAction.get keyMap getKey

        actual |> shouldEqual <| PocofData.None

[<EntryPoint>]
let main argv = 0
