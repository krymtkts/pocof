module Pocof

open System
open System.Management.Automation

open Xunit
open FsUnitTyped

open Pocof.Pocof
open Pocof.Data
open Pocof.Screen
open PocofUI

let toObj x = x |> (PSObject.AsPSObject >> Obj)

let mapToObj x = x |> List.map toObj

let initState () : InternalState =
    { QueryState =
        { Query = ""
          Cursor = 0
          WindowBeginningCursor = 0
          WindowWidth = 0 }
      QueryCondition =
        { Matcher = Matcher.MATCH
          Operator = Operator.OR
          CaseSensitive = false
          Invert = false }
      PropertySearch = PropertySearch.NoSearch
      Notification = ""
      SuppressProperties = false
      Properties = [ "Name"; "LastModified"; "Path" ]
      Prompt = "query"
      FilteredCount = 0
      ConsoleWidth = 60
      Refresh = Refresh.Required }

let state = initState ()
let writeScreen _ _ _ = ()

let pos = { Y = 0; Height = 0 } // NOTE: not used in this test.

let propMap = Map.empty

let results = [ "a"; "b"; "c"; "d"; "e" ] |> List.map box

module calculateWindowBeginningCursor =
    [<Fact>]
    let ``should return 0.`` () =
        let state =
            { Query = "a"
              Cursor = 1
              WindowBeginningCursor = 0
              WindowWidth = 30 }

        let rui = new MockRawUI()
        use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let actual = calculateWindowBeginningCursor buff.GetLengthInBufferCells state

        actual |> shouldEqual 0

    [<Fact>]
    let ``should return 1.`` () =
        let state =
            { Query = String.replicate 31 "a"
              Cursor = 31
              WindowBeginningCursor = 0
              WindowWidth = 30 }

        let rui = new MockRawUI()
        use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let actual = calculateWindowBeginningCursor buff.GetLengthInBufferCells state

        actual |> shouldEqual 1

    [<Fact>]
    let ``should return cursor value.`` () =
        let state =
            { Query = String.replicate 31 "a"
              Cursor = 0
              WindowBeginningCursor = 31
              WindowWidth = 30 }

        let rui = new MockRawUI()
        use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let actual = calculateWindowBeginningCursor buff.GetLengthInBufferCells state

        actual |> shouldEqual 0

module loop =

    [<Fact>]
    let ``should return result when finishing.`` () =
        let input = results |> List.map toObj
        let state, context = Pocof.Query.prepare state

        let rui = new MockRawUI(60, 30, [ MockRawUI.consoleKey '\000' ConsoleKey.Enter ])
        use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let args =
            { keymaps = Pocof.Keys.defaultKeymap
              input = input
              propMap = propMap
              writeScreen = writeScreen
              getKey = buff.getKey
              getConsoleWidth = buff.getConsoleWidth
              getLengthInBufferCells = String.length }

        let actual = loop args input state pos context
        actual |> List.length |> shouldEqual 5

        actual
        |> List.iteri (fun i x -> x = results.[i] |> shouldEqual true)

        rui.check ()

    [<Fact>]
    let ``shouldn't return result when canceling.`` () =
        let input = results |> List.map toObj

        let state, context = Pocof.Query.prepare { state with SuppressProperties = true }

        let rui = new MockRawUI(60, 30, [ MockRawUI.consoleKey '\000' ConsoleKey.Escape ])
        use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let args =
            { keymaps = Pocof.Keys.defaultKeymap
              input = input
              propMap = propMap
              writeScreen = writeScreen
              getKey = buff.getKey
              getConsoleWidth = buff.getConsoleWidth
              getLengthInBufferCells = String.length }

        let actual = loop args input state pos context
        actual |> List.length |> shouldEqual 0
        rui.check ()

    [<Fact>]
    let ``should return result when finishing after noop.`` () =
        let input = results |> List.map toObj

        let state, context =
            Pocof.Query.prepare { state with Refresh = Refresh.NotRequired }

        let rui =
            new MockRawUI(
                60,
                30,
                [ new ConsoleKeyInfo('\000', ConsoleKey.Escape, true, true, false)
                  |> Some
                  None
                  MockRawUI.consoleKey '\000' ConsoleKey.Enter ]
            )

        use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let args =
            { keymaps = Pocof.Keys.defaultKeymap
              input = input
              propMap = propMap
              writeScreen = writeScreen
              getKey = buff.getKey
              getConsoleWidth = buff.getConsoleWidth
              getLengthInBufferCells = String.length }

        let actual = loop args input state pos context
        actual |> List.length |> shouldEqual 5

        actual
        |> List.iteri (fun i x -> x = results.[i] |> shouldEqual true)

        rui.check ()

    [<Fact>]
    let ``should return result when finishing with filter.`` () =
        let input = results |> List.map toObj
        let state, context = Pocof.Query.prepare state

        let rui =
            new MockRawUI(
                60,
                30,
                [ MockRawUI.consoleKey 'a' ConsoleKey.A
                  MockRawUI.consoleKey ' ' ConsoleKey.Spacebar
                  MockRawUI.consoleKey 'd' ConsoleKey.D
                  None
                  MockRawUI.consoleKey '\000' ConsoleKey.Enter ]
            )

        use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let args =
            { keymaps = Pocof.Keys.defaultKeymap
              input = input
              propMap = propMap
              writeScreen = writeScreen
              getKey = buff.getKey
              getConsoleWidth = buff.getConsoleWidth
              getLengthInBufferCells = String.length }

        let actual = loop args input state pos context
        actual |> List.length |> shouldEqual 2
        actual.[0] = results.[0] |> shouldEqual true
        actual.[1] = results.[3] |> shouldEqual true
        rui.check ()

    [<Fact>]
    let ``should update QueryState.WindowWidth based on ConsoleWidth.`` () =
        let input = results |> List.map toObj

        let state, context = Pocof.Query.prepare { state with SuppressProperties = true }

        let rui =
            new MockRawUI(
                60,
                30,
                [ MockRawUI.consoleKey 'a' ConsoleKey.A
                  None
                  MockRawUI.consoleKey '\000' ConsoleKey.Enter ]
            )

        use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let args =
            { keymaps = Pocof.Keys.defaultKeymap
              input = input
              propMap = propMap
              writeScreen = buff.writeScreen Layout.TopDown
              getKey = buff.getKey
              getConsoleWidth =
                fun () ->
                    rui.width <- 80
                    80
              getLengthInBufferCells = String.length }

        let actual = loop args input state pos context
        actual |> List.length |> shouldEqual 1
        actual.[0] = results.[0] |> shouldEqual true

        let expected: string list =
            $"""query>a{String.replicate 60 " "} match or [1]"""
            :: (generateLine 80 (rui.height - 1))

        rui.screen |> shouldEqual expected
        rui.check ()

module interact =
    [<Fact>]
    let ``should return result with NonInteractive mode.`` () =
        let config: InternalConfig =
            { NotInteractive = true
              Layout = Layout.TopDown
              Keymaps = Pocof.Keys.defaultKeymap }

        let input = results |> List.map toObj
        let pos = { Y = 0; Height = 0 }
        let rui = new MockRawUI()

        let actual = interact config state pos (fun () -> rui) (fun _ -> Seq.empty) input
        actual |> List.length |> shouldEqual 5

        let expected =
            [ "a"; "b"; "c"; "d"; "e" ]
            |> List.map (PSObject.AsPSObject >> box)

        actual |> shouldEqual expected

    [<Fact>]
    let ``should return result when interaction finished in Interactive mode and TopDown Layout.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.TopDown
              Keymaps = Pocof.Keys.defaultKeymap }

        let input = results |> List.map toObj
        let pos = { Y = 0; Height = 0 }
        let rui = new MockRawUI()

        let actual = interact config state pos (fun () -> rui) (fun _ -> Seq.empty) input
        actual |> List.length |> shouldEqual 5

        let expected =
            [ "a"; "b"; "c"; "d"; "e" ]
            |> List.map (PSObject.AsPSObject >> box)

        actual |> shouldEqual expected


    [<Fact>]
    let ``should return result when interaction finished in Interactive mode and BottomUp Layout.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUp
              Keymaps = Pocof.Keys.defaultKeymap }

        let input = results |> List.map toObj
        let pos = { Y = 0; Height = 0 }
        let rui = new MockRawUI()

        let actual = interact config state pos (fun () -> rui) (fun _ -> Seq.empty) input
        actual |> List.length |> shouldEqual 5

        let expected =
            [ "a"; "b"; "c"; "d"; "e" ]
            |> List.map (PSObject.AsPSObject >> box)

        actual |> shouldEqual expected

    [<Fact>]
    let ``should return result when interaction finished in Interactive mode and BottomUpHalp Layout.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUpHalf
              Keymaps = Pocof.Keys.defaultKeymap }

        let input = results |> List.map toObj
        let pos = { Y = 0; Height = 0 }
        let rui = new MockRawUI()

        let actual = interact config state pos (fun () -> rui) (fun _ -> Seq.empty) input
        actual |> List.length |> shouldEqual 5

        let expected =
            [ "a"; "b"; "c"; "d"; "e" ]
            |> List.map (PSObject.AsPSObject >> box)

        actual |> shouldEqual expected

module buildInput =
    open System.Collections

    let mapToObj x =
        x |> List.map (PSObject.AsPSObject >> Obj)

    [<Fact>]
    let ``should return the list with added Obj`` () =
        let expected = [ 3; 2; 1 ] |> mapToObj

        buildInput
            []
            ([ 1; 2; 3 ]
             |> List.map PSObject.AsPSObject
             |> Array.ofList)
        |> shouldEqual expected

    [<Fact>]
    let ``should return the list with added Obj to head.`` () =
        let expected = [ 3; 2; 1; 0 ] |> mapToObj

        let input = [ 0 ] |> mapToObj

        buildInput
            input
            ([ 1; 2; 3 ]
             |> List.map PSObject.AsPSObject
             |> Array.ofList)
        |> shouldEqual expected

    [<Fact>]
    let ``should return the list with added Dict`` () =
        let expected =
            [ DictionaryEntry("c", 3)
              DictionaryEntry("b", 2)
              DictionaryEntry("a", 1) ]
            |> List.map Dict

        let inputObject =
            let h = new OrderedHashtable()
            h.Add("a", 1)
            h.Add("b", 2)
            h.Add("c", 3)
            [| h |> PSObject.AsPSObject |]

        buildInput [] inputObject |> shouldEqual expected

module buildProperties =
    [<Fact>]
    let ``should return the set with added input properties.`` () =
        let expected = set [ "a"; "b"; "c" ]
        let input = set []

        let inputObject =
            let o = new PSObject()
            o.Properties.Add(new PSNoteProperty("a", 1))
            o.Properties.Add(new PSNoteProperty("b", 2))
            o.Properties.Add(new PSNoteProperty("c", 3))
            [| o |]

        buildProperties input inputObject
        |> shouldEqual expected

    [<Fact>]
    let ``should return the set with added input properties without duplication.`` () =
        let expected = set [ "a"; "b"; "c" ]
        let input = set [ "a"; "c" ]

        let inputObject =
            let o = new PSObject()
            o.Properties.Add(new PSNoteProperty("a", 1))
            o.Properties.Add(new PSNoteProperty("b", 2))
            o.Properties.Add(new PSNoteProperty("c", 3))
            [| o |]

        buildProperties input inputObject
        |> shouldEqual expected

    [<Fact>]
    let ``should return the set with added the keys of hashtable.`` () =
        let expected = [ "Key"; "Value" ] |> Set.ofList

        let input = set []

        let inputObject =
            let h = new OrderedHashtable()
            h.Add("a", 1)
            [| h |> PSObject.AsPSObject |]

        buildProperties input inputObject
        |> shouldEqual expected
