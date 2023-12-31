module Pocof

open Xunit
open FsUnitTyped
open System
open pocof.Pocof
open pocof.PocofData

module loop =
    open System.Management.Automation

    let initState () : InternalState =
        { QueryState =
            { Query = ""
              Cursor = 0
              WindowBeginningX = 0
              WindowWidth = 0 }
          QueryCondition =
            { Matcher = MATCH
              Operator = OR
              CaseSensitive = false
              Invert = false }
          PropertySearch = NoSearch
          Notification = ""
          SuppressProperties = false
          Properties = [ "Name"; "LastModified"; "Path" ]
          Refresh = Required }

    let state = initState ()
    let writeScreen _ _ _ _ = ()

    let pos = { Y = 0; Height = 0 } // NOTE: not used in this test.

    let propMap = Map.empty

    let toObj = PSObject.AsPSObject >> Obj

    let results = [ "a"; "b"; "c"; "d"; "e" ] |> List.map box

    type MockGetKey(keys: ConsoleKeyInfo list list) =
        let mutable keys = keys

        member __.getKey() =
            match keys with
            | [] -> failwith "no keys remains. probably test is broken."
            | x :: xs ->
                keys <- xs
                x

        member __.check() =
            match keys with
            | [] -> ()
            | _ -> failwith "keys remains. probably test is broken."

    [<Fact>]
    let ``should return result when finishing.`` () =
        let input = results |> List.map toObj
        let state, context = pocof.PocofQuery.prepare state

        let m =
            MockGetKey [ [ new ConsoleKeyInfo('\000', ConsoleKey.Enter, false, false, false) ] ]

        let args =
            { keymaps = pocof.PocofAction.defaultKeymap
              input = input
              propMap = propMap
              writeScreen = writeScreen
              getKey = m.getKey }

        let actual = loop args input state pos context
        actual |> List.length |> shouldEqual 5

        actual
        |> List.iteri (fun i x -> x = results.[i] |> shouldEqual true)

    [<Fact>]
    let ``shouldn't return result when canceling.`` () =
        let input = results |> List.map toObj

        let state, context =
            pocof.PocofQuery.prepare { state with SuppressProperties = true }

        let m =
            MockGetKey [ [ new ConsoleKeyInfo('\000', ConsoleKey.Escape, false, false, false) ] ]

        let args =
            { keymaps = pocof.PocofAction.defaultKeymap
              input = input
              propMap = propMap
              writeScreen = writeScreen
              getKey = m.getKey }

        let actual = loop args input state pos context
        actual |> List.length |> shouldEqual 0

    [<Fact>]
    let ``should return result when finishing after noop.`` () =
        let input = results |> List.map toObj

        let state, context = pocof.PocofQuery.prepare { state with Refresh = NotRequired }

        let m =
            MockGetKey [ [ new ConsoleKeyInfo('\000', ConsoleKey.Escape, true, true, false) ]
                         [ new ConsoleKeyInfo('\000', ConsoleKey.Enter, false, false, false) ] ]

        let args =
            { keymaps = pocof.PocofAction.defaultKeymap
              input = input
              propMap = propMap
              writeScreen = writeScreen
              getKey = m.getKey }

        let actual = loop args input state pos context
        actual |> List.length |> shouldEqual 5

        actual
        |> List.iteri (fun i x -> x = results.[i] |> shouldEqual true)

        m.check ()

    [<Fact>]
    let ``should return result when finishing with filter.`` () =
        let input = results |> List.map toObj

        let state, context = pocof.PocofQuery.prepare state

        let m =
            MockGetKey [ [ new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false)
                           new ConsoleKeyInfo(' ', ConsoleKey.Spacebar, false, false, false)
                           new ConsoleKeyInfo('d', ConsoleKey.D, false, false, false) ]
                         [ new ConsoleKeyInfo('\000', ConsoleKey.Enter, false, false, false) ] ]

        let args =
            { keymaps = pocof.PocofAction.defaultKeymap
              input = input
              propMap = propMap
              writeScreen = writeScreen
              getKey = m.getKey }

        let actual = loop args input state pos context
        actual |> List.length |> shouldEqual 2
        actual.[0] = results.[0] |> shouldEqual true
        actual.[1] = results.[3] |> shouldEqual true
        m.check ()
