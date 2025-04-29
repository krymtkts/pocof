module PocofTest.Keys

open System
open System.Collections
open System.Management.Automation

open Xunit
open FsUnitTyped
open Expecto
open Expecto.Flip

open Pocof

[<Tests>]
let tests_toKeyPattern =
    testList
        "toKeyPattern"
        [

          test "A without modifiers." {
              Keys.toKeyPattern "A"
              |> Expect.equal "A without modifiers." (Ok { Modifier = 0; Key = ConsoleKey.A })
          }

          test "A with Alt." {
              Keys.toKeyPattern "alt+a"
              |> Expect.equal "A with Alt." (Ok { Modifier = 1; Key = ConsoleKey.A })
          }

          test "A with Alt and Shift." {
              Keys.toKeyPattern "Alt+Shift+A"
              |> Expect.equal "A with Alt and Shift." (Ok { Modifier = 3; Key = ConsoleKey.A })
          }

          test "A with Ctrl, Alt and Shift." {
              Keys.toKeyPattern "control+alt+shift+A"
              |> Expect.equal "A with Ctrl, Alt and Shift." (Ok { Modifier = 7; Key = ConsoleKey.A })
          }

          test "Error when empty." {
              Keys.toKeyPattern ""
              |> Expect.equal "Error when empty." (Error "Unsupported key ''.")
          }

          test "Error when unsupported combination." {
              Keys.toKeyPattern "cnt+alt+c+ESC"
              |> Expect.equal
                  "Error when unsupported combination."
                  (Error "Unsupported modifier 'cnt'. Unsupported modifier 'c'. Unsupported key 'ESC'.")
          }

          ]

[<Tests>]
let tests_get =
    testList
        "get"
        [

          test "PocofData.AddQuery if no modifier is specified." {
              let key = [ new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false) ]
              let actual = Keys.get Map.empty key
              actual |> Expect.equal "PocofData.AddQuery" (Data.Action.AddQuery "a")
          }

          test "PocofData.AddQuery if symbol with shift." {
              let getKey = [ new ConsoleKeyInfo(':', ConsoleKey.Oem1, true, false, false) ]
              let actual = Keys.get Map.empty getKey

              actual
              |> Expect.equal "PocofData.AddQuery if symbol with shift." (Data.Action.AddQuery ":")
          }

          test "PocofData.AddQuery multiple times." {
              let getKey =
                  [ new ConsoleKeyInfo('p', ConsoleKey.P, false, false, false)
                    new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false)
                    new ConsoleKeyInfo('s', ConsoleKey.S, false, false, false)
                    new ConsoleKeyInfo('t', ConsoleKey.T, false, false, false)
                    new ConsoleKeyInfo('e', ConsoleKey.E, false, false, false) ]

              let actual = Keys.get Map.empty getKey

              actual
              |> Expect.equal "PocofData.AddQuery multiple times." (Data.Action.AddQuery "paste")
          }

          test "user-defined Action if matched." {
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
              actual |> Expect.equal "user-defined Action if matched." Data.Action.Finish
          }

          test "Action if matched." {
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
              |> Expect.equal "Action if matched (DeleteBackwardInput)" Data.Action.DeleteBackwardInput

              let actual =
                  Keys.get keyMap [ new ConsoleKeyInfo('e', ConsoleKey.E, true, true, true) ]

              actual |> Expect.equal "Action if matched (Finish)" Data.Action.Finish

              let actual =
                  Keys.get keyMap [ new ConsoleKeyInfo('\000', ConsoleKey.Escape, false, true, false) ]

              actual |> Expect.equal "Action if matched (Noop)" Data.Action.Noop
          }

          test "Action if matched to no modifier key." {
              let keu = [ new ConsoleKeyInfo('a', ConsoleKey.Home, false, false, false) ]
              let actual = Keys.get Keys.defaultKeymap keu

              actual
              |> Expect.equal "Action if matched to no modifier key." Data.Action.BeginningOfLine
          }

          test "PocofData.AddQuery if not match the keymap." {
              let keyMap: Map<Data.KeyPattern, Data.Action> =
                  Map [ ({ Modifier = 1; Key = ConsoleKey.U }, Data.Action.DeleteBackwardInput) ]

              let key = [ new ConsoleKeyInfo('u', ConsoleKey.U, false, true, true) ]
              let actual = Keys.get keyMap key

              actual
              |> Expect.equal "PocofData.AddQuery if not match the keymap." (Data.Action.AddQuery "u")
          }

          test "PocofData.None if the control character not match the keymap." {
              let key = [ new ConsoleKeyInfo('\009', ConsoleKey.Tab, false, true, true) ]
              let actual = Keys.get Keys.defaultKeymap key

              actual
              |> Expect.equal "PocofData.None if the control character not match the keymap." Data.Action.Noop
          }

          test "None if not match the keymap." {
              let key = [ new ConsoleKeyInfo('\000', ConsoleKey.F1, false, false, false) ]
              let actual = Keys.get Keys.defaultKeymap key
              actual |> Expect.equal "None if not match the keymap." Data.Action.Noop
          }

          ]

[<Tests>]
let tests_convertKeymaps =
    testList
        "convertKeymaps"
        [

          test "map transformed from hashtable" {
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

              Keys.convertKeymaps h |> Expect.equal "map transformed from hashtable" expected
          }

          test "error if the hashtable contains invalid key or action." {
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

              Keys.convertKeymaps h
              |> Expect.equal "error if the hashtable contains invalid key or action." expected
          }

          test "default map from null hashtable" {
              let expected = Keys.defaultKeymap |> Ok

              Keys.convertKeymaps null
              |> Expect.equal "default map from null hashtable" expected
          }

          ]
