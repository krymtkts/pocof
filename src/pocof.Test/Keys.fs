module PocofTest.Keys

open System
open System.Collections
open System.Management.Automation

open Expecto
open Expecto.Flip

open Pocof

[<Tests>]
let tests_toKeyPattern =
    testList
        "toKeyPattern"
        [

          test "When input is 'A' (no modifiers)" {
              Keys.toKeyPattern "A"
              |> Expect.wantOk "should return Ok"
              |> Expect.equal "should return no modifiers" { Modifier = 0; Key = ConsoleKey.A }
          }

          test "When input is 'alt+a'" {
              Keys.toKeyPattern "alt+a"
              |> Expect.wantOk "should return Ok"
              |> Expect.equal "should return Alt modifier" { Modifier = 1; Key = ConsoleKey.A }
          }

          test "When input is 'Alt+Shift+A'" {
              Keys.toKeyPattern "Alt+Shift+A"
              |> Expect.wantOk "should return Ok"
              |> Expect.equal "should return Alt and Shift modifiers" { Modifier = 3; Key = ConsoleKey.A }
          }

          test "When input is 'control+alt+shift+A'" {
              Keys.toKeyPattern "control+alt+shift+A"
              |> Expect.wantOk "should return Ok"
              |> Expect.equal "should return Ctrl, Alt and Shift modifiers" { Modifier = 7; Key = ConsoleKey.A }
          }

          test "When input is empty string" {
              Keys.toKeyPattern ""
              |> Expect.wantError "should return Error"
              |> Expect.equal "should return error message for empty string" "Unsupported key ''."
          }

          test "When input is unsupported combination" {
              Keys.toKeyPattern "cnt+alt+c+ESC"
              |> Expect.wantError "should return Error"
              |> Expect.equal
                  "should return error message for unsupported combination"
                  "Unsupported modifier 'cnt'. Unsupported modifier 'c'. Unsupported key 'ESC'."
          }

          ]

[<Tests>]
let tests_get =
    testList
        "get"
        [

          test "When no modifier" {
              let key = [ new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false) ]
              let actual = Keys.get Map.empty key
              actual |> Expect.equal "should return AddQuery" (Data.Action.AddQuery "a")
          }

          test "When symbol with shift" {
              let getKey = [ new ConsoleKeyInfo(':', ConsoleKey.Oem1, true, false, false) ]
              let actual = Keys.get Map.empty getKey

              actual
              |> Expect.equal "should return AddQuery for symbol with shift" (Data.Action.AddQuery ":")
          }

          test "When multiple keys are pressed" {
              let getKey =
                  [ new ConsoleKeyInfo('p', ConsoleKey.P, false, false, false)
                    new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false)
                    new ConsoleKeyInfo('s', ConsoleKey.S, false, false, false)
                    new ConsoleKeyInfo('t', ConsoleKey.T, false, false, false)
                    new ConsoleKeyInfo('e', ConsoleKey.E, false, false, false) ]

              let actual = Keys.get Map.empty getKey

              actual
              |> Expect.equal "should return AddQuery for multiple keys" (Data.Action.AddQuery "paste")
          }

          test "When user-defined Action is matched" {
              let keyMap: Map<Data.KeyPattern, Data.Action> =
                  Map
                      [ { Data.KeyPattern.Modifier = 7
                          Data.KeyPattern.Key = ConsoleKey.E },
                        Data.Action.Finish
                        { Data.KeyPattern.Modifier = 0
                          Data.KeyPattern.Key = ConsoleKey.Escape },
                        Data.Action.Noop ]

              let key = [ new ConsoleKeyInfo('e', ConsoleKey.E, true, true, true) ]
              let actual = Keys.get keyMap key
              actual |> Expect.equal "should return user-defined Action" Data.Action.Finish
          }

          test "When Action is matched (DeleteBackwardInput, Finish, Noop)" {
              let keyMap: Map<Data.KeyPattern, Data.Action> =
                  ([ { Data.KeyPattern.Modifier = 7
                       Data.KeyPattern.Key = ConsoleKey.E }
                     { Data.KeyPattern.Modifier = 0
                       Data.KeyPattern.Key = ConsoleKey.Escape } ],
                   [ Data.Action.Finish; Data.Action.Noop ],
                   Keys.defaultKeymap)
                  |||> List.foldBack2 Map.add

              let actual =
                  Keys.get keyMap [ new ConsoleKeyInfo('\000', ConsoleKey.Home, false, false, true) ]

              actual
              |> Expect.equal "should return DeleteBackwardInput" Data.Action.DeleteBackwardInput

              let actual =
                  Keys.get keyMap [ new ConsoleKeyInfo('e', ConsoleKey.E, true, true, true) ]

              actual |> Expect.equal "should return Finish" Data.Action.Finish

              let actual =
                  Keys.get keyMap [ new ConsoleKeyInfo('\000', ConsoleKey.Escape, false, true, false) ]

              actual |> Expect.equal "should return Noop" Data.Action.Noop
          }

          test "When matched to no modifier key" {
              let keu = [ new ConsoleKeyInfo('a', ConsoleKey.Home, false, false, false) ]
              let actual = Keys.get Keys.defaultKeymap keu

              actual
              |> Expect.equal "should return BeginningOfLine" Data.Action.BeginningOfLine
          }

          test "When not matched in keymap" {
              let keyMap: Map<Data.KeyPattern, Data.Action> =
                  Map [ ({ Modifier = 1; Key = ConsoleKey.U }, Data.Action.DeleteBackwardInput) ]

              let key = [ new ConsoleKeyInfo('u', ConsoleKey.U, false, true, true) ]
              let actual = Keys.get keyMap key

              actual
              |> Expect.equal "should return AddQuery for unmatched key" (Data.Action.AddQuery "u")
          }

          test "When control character not matched in keymap" {
              let key = [ new ConsoleKeyInfo('\009', ConsoleKey.Tab, false, true, true) ]
              let actual = Keys.get Keys.defaultKeymap key

              actual
              |> Expect.equal "should return Noop for unmatched control character" Data.Action.Noop
          }

          test "When not matched in keymap (Noop)" {
              let key = [ new ConsoleKeyInfo('\000', ConsoleKey.F1, false, false, false) ]
              let actual = Keys.get Keys.defaultKeymap key
              actual |> Expect.equal "should return Noop for unmatched key" Data.Action.Noop
          }

          ]

[<Tests>]
let tests_convertKeymaps =
    testList
        "convertKeymaps"
        [

          test "When hashtable" {
              let h = new Hashtable()
              h.Add("control+alt+shift+x", "cancel")
              h.Add("ESCAPE", "NOOP")

              let expected =
                  ([ { Data.KeyPattern.Modifier = 7
                       Data.KeyPattern.Key = ConsoleKey.X }
                     { Data.KeyPattern.Modifier = 0
                       Data.KeyPattern.Key = ConsoleKey.Escape } ],
                   [ Data.Action.Cancel; Data.Action.Noop ],
                   Keys.defaultKeymap)
                  |||> List.foldBack2 Map.add
                  |> Ok

              Keys.convertKeymaps h
              |> Expect.equal "should return map transformed from hashtable" expected
          }

          test "When hashtable contains invalid key or action" {
              let h = new OrderedHashtable()
              h.Add("contrl+x", "cancel")
              h.Add("alte+a", "Finissh")
              h.Add("control+alt+shift+x", "cancel")
              h.Add("ESCAE", "NOOP")
              h.Add("TAB", "CompleteProperties")

              let expected =
                  [ "Unsupported modifier 'contrl'."
                    "Unsupported modifier 'alte'.Unknown Action 'Finissh'."
                    "Unsupported key 'ESCAE'."
                    "Unknown Action 'CompleteProperties'." ]
                  |> String.concat "\n"
                  |> Error

              Keys.convertKeymaps h |> Expect.equal "should return error" expected
          }

          test "When hashtable is null" {
              let expected = Keys.defaultKeymap |> Ok

              Keys.convertKeymaps null |> Expect.equal "should return default map" expected
          }

          ]
