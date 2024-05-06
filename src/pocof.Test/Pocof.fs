module PocofTest.Pocof

open System
open System.Management.Automation

open Xunit
open FsUnitTyped

open Pocof
open Pocof.Data
open Screen.Mock

let toObj x = x |> (PSObject.AsPSObject >> Entry.Obj)

let mapToObj x = x |> List.map toObj

let initState () : InternalState =
    { QueryState =
        { Query = ""
          Cursor = 0
          WindowBeginningCursor = 0
          WindowWidth = 0
          InputMode = InputMode.Input }
      QueryCondition =
        { Matcher = Matcher.Match
          Operator = Operator.Or
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
              WindowWidth = 30
              InputMode = InputMode.Input }

        let rui = new MockRawUI()
        use buff = new Screen.Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let actual = Pocof.calculateWindowBeginningCursor buff.GetLengthInBufferCells state

        actual |> shouldEqual 0

    [<Fact>]
    let ``should return 1.`` () =
        let state =
            { Query = String.replicate 31 "a"
              Cursor = 31
              WindowBeginningCursor = 0
              WindowWidth = 30
              InputMode = InputMode.Input }

        let rui = new MockRawUI()
        use buff = new Screen.Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let actual = Pocof.calculateWindowBeginningCursor buff.GetLengthInBufferCells state

        actual |> shouldEqual 1

    [<Fact>]
    let ``should return cursor value.`` () =
        let state =
            { Query = String.replicate 31 "a"
              Cursor = 0
              WindowBeginningCursor = 31
              WindowWidth = 30
              InputMode = InputMode.Input }

        let rui = new MockRawUI()
        use buff = new Screen.Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let actual = Pocof.calculateWindowBeginningCursor buff.GetLengthInBufferCells state

        actual |> shouldEqual 0

module loop =

    [<Fact>]
    let ``should return result when finishing.`` () =
        let input = results |> List.map toObj
        let state, context = Query.prepare state

        let rui = new MockRawUI(60, 30, [ MockRawUI.ConsoleKey '\000' ConsoleKey.Enter ])
        use buff = new Screen.Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let args: Pocof.LoopFixedArguments =
            { Keymaps = Keys.defaultKeymap
              Input = input
              PropMap = propMap
              WriteScreen = writeScreen
              GetKey = buff.GetKey
              GetConsoleWidth = buff.GetConsoleWidth
              GetLengthInBufferCells = String.length }

        let actual = Pocof.loop args input state pos context
        actual |> Seq.length |> shouldEqual 5

        actual |> Seq.iteri (fun i x -> x = results.[i] |> shouldEqual true)

        rui.Check()

    [<Fact>]
    let ``shouldn't return result when canceling.`` () =
        let input = results |> List.map toObj

        let state, context = Query.prepare { state with SuppressProperties = true }

        let rui = new MockRawUI(60, 30, [], true)
        use buff = new Screen.Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let args: Pocof.LoopFixedArguments =
            { Keymaps = Keys.defaultKeymap
              Input = input
              PropMap = propMap
              WriteScreen = writeScreen
              GetKey = buff.GetKey
              GetConsoleWidth = buff.GetConsoleWidth
              GetLengthInBufferCells = String.length }

        let actual = Pocof.loop args input state pos context
        actual |> Seq.length |> shouldEqual 0
        rui.Check()

    [<Fact>]
    let ``should return result when finishing after noop.`` () =
        let input = results |> List.map toObj

        let state, context =
            Query.prepare
                { state with
                    Refresh = Refresh.NotRequired }

        let rui =
            new MockRawUI(
                60,
                30,
                [ new ConsoleKeyInfo('\000', ConsoleKey.Escape, true, true, false) |> Some
                  None
                  MockRawUI.ConsoleKey '\000' ConsoleKey.Enter ]
            )

        use buff = new Screen.Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let args: Pocof.LoopFixedArguments =
            { Keymaps = Keys.defaultKeymap
              Input = input
              PropMap = propMap
              WriteScreen = writeScreen
              GetKey = buff.GetKey
              GetConsoleWidth = buff.GetConsoleWidth
              GetLengthInBufferCells = String.length }

        let actual = Pocof.loop args input state pos context
        actual |> Seq.length |> shouldEqual 5

        actual |> Seq.iteri (fun i x -> x = results.[i] |> shouldEqual true)

        rui.Check()

    [<Fact>]
    let ``should return result when finishing with filter.`` () =
        let input = results |> List.map toObj
        let state, context = Query.prepare state

        let rui =
            new MockRawUI(
                60,
                30,
                [ MockRawUI.ConsoleKey 'a' ConsoleKey.A
                  MockRawUI.ConsoleKey ' ' ConsoleKey.Spacebar
                  MockRawUI.ConsoleKey 'd' ConsoleKey.D
                  None
                  MockRawUI.ConsoleKey '\000' ConsoleKey.Enter ]
            )

        use buff = new Screen.Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let args: Pocof.LoopFixedArguments =
            { Keymaps = Keys.defaultKeymap
              Input = input
              PropMap = propMap
              WriteScreen = writeScreen
              GetKey = buff.GetKey
              GetConsoleWidth = buff.GetConsoleWidth
              GetLengthInBufferCells = String.length }

        let actual = Pocof.loop args input state pos context
        actual |> Seq.length |> shouldEqual 2
        Seq.item 0 actual = results.[0] |> shouldEqual true
        Seq.item 1 actual = results.[3] |> shouldEqual true
        rui.Check()

    [<Fact>]
    let ``should update QueryState.WindowWidth based on ConsoleWidth.`` () =
        let input = results |> List.map toObj

        let state, context = Query.prepare { state with SuppressProperties = true }

        let rui =
            new MockRawUI(
                60,
                30,
                [ MockRawUI.ConsoleKey 'a' ConsoleKey.A
                  None
                  MockRawUI.ConsoleKey '\000' ConsoleKey.Enter ]
            )

        use buff = new Screen.Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let args: Pocof.LoopFixedArguments =
            { Keymaps = Keys.defaultKeymap
              Input = input
              PropMap = propMap
              WriteScreen = buff.WriteScreen Layout.TopDown
              GetKey = buff.GetKey
              GetConsoleWidth =
                fun () ->
                    rui.width <- 80
                    80
              GetLengthInBufferCells = String.length }

        let actual = Pocof.loop args input state pos context
        actual |> Seq.length |> shouldEqual 1
        Seq.item 0 actual = results.[0] |> shouldEqual true

        let expected: string list =
            $"""query>a{String.replicate 60 " "} match or [1]"""
            :: (generateLine 80 (rui.height - 1))

        rui.screen |> shouldEqual expected
        rui.Check()

module interact =
    [<Fact>]
    let ``should return result with NonInteractive mode.`` () =
        let config: InternalConfig =
            { NotInteractive = true
              Layout = Layout.TopDown
              Keymaps = Keys.defaultKeymap }

        let input = results |> List.map toObj
        let pos = { Y = 0; Height = 0 }
        let rui = new MockRawUI()

        let actual =
            Pocof.interact config state pos (fun () -> rui) (fun _ -> Seq.empty) input

        actual |> Seq.length |> shouldEqual 5

        let expected = [ "a"; "b"; "c"; "d"; "e" ] |> List.map (PSObject.AsPSObject >> box)

        actual |> List.ofSeq |> shouldEqual expected

    [<Fact>]
    let ``should return result when interaction finished in Interactive mode and TopDown Layout.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.TopDown
              Keymaps = Keys.defaultKeymap }

        let input = results |> List.map toObj
        let pos = { Y = 0; Height = 0 }
        let rui = new MockRawUI()

        let actual =
            Pocof.interact config state pos (fun () -> rui) (fun _ -> Seq.empty) input

        actual |> Seq.length |> shouldEqual 5

        let expected = [ "a"; "b"; "c"; "d"; "e" ] |> List.map (PSObject.AsPSObject >> box)

        actual |> List.ofSeq |> shouldEqual expected


    [<Fact>]
    let ``should return result when interaction finished in Interactive mode and BottomUp Layout.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUp
              Keymaps = Keys.defaultKeymap }

        let input = results |> List.map toObj
        let pos = { Y = 0; Height = 0 }
        let rui = new MockRawUI()

        let actual =
            Pocof.interact config state pos (fun () -> rui) (fun _ -> Seq.empty) input

        actual |> Seq.length |> shouldEqual 5

        let expected = [ "a"; "b"; "c"; "d"; "e" ] |> List.map (PSObject.AsPSObject >> box)

        actual |> List.ofSeq |> shouldEqual expected

    [<Fact>]
    let ``should return result when interaction finished in Interactive mode and BottomUpHalp Layout.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUpHalf
              Keymaps = Keys.defaultKeymap }

        let input = results |> List.map toObj
        let pos = { Y = 0; Height = 0 }
        let rui = new MockRawUI()

        let actual =
            Pocof.interact config state pos (fun () -> rui) (fun _ -> Seq.empty) input

        actual |> Seq.length |> shouldEqual 5

        let expected = [ "a"; "b"; "c"; "d"; "e" ] |> List.map (PSObject.AsPSObject >> box)

        actual |> List.ofSeq |> shouldEqual expected

module buildInput =
    open System.Collections

    let mapToObj x =
        x |> List.map (PSObject.AsPSObject >> Entry.Obj)

    [<Fact>]
    let ``should return the list with added Obj`` () =
        let expected = [ 1; 2; 3 ] |> mapToObj
        let input: Pocof.Entry Generic.List = Generic.List()

        Pocof.addInput input.Add ([| 1; 2; 3 |] |> Array.map PSObject.AsPSObject)

        input |> List.ofSeq |> shouldEqual expected

    // [<Fact>]
    // let ``should return the list with added Obj to head.`` () =
    //     let expected = [ 3; 2; 1; 0 ] |> mapToObj

    //     Pocof.buildInput  ([ 1; 2; 3 ] |> List.map PSObject.AsPSObject |> Array.ofList)
    //     |> shouldEqual expected

    [<Fact>]
    let ``should return the list with added Dict`` () =
        let expected =
            [ DictionaryEntry("a", 1); DictionaryEntry("b", 2); DictionaryEntry("c", 3) ]
            |> List.map Entry.Dict

        let input: Pocof.Entry Generic.List = Generic.List()

        let inputObject =
            let h = new OrderedHashtable()
            h.Add("a", 1)
            h.Add("b", 2)
            h.Add("c", 3)
            [| h |> PSObject.AsPSObject |]

        Pocof.addInput input.Add inputObject
        input |> List.ofSeq |> shouldEqual expected

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

        Pocof.buildProperties input inputObject |> shouldEqual expected

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

        Pocof.buildProperties input inputObject |> shouldEqual expected

    [<Fact>]
    let ``should return the set with added the keys of hashtable.`` () =
        let expected = [ "Key"; "Value" ] |> Set.ofList

        let input = set []

        let inputObject =
            let h = new OrderedHashtable()
            h.Add("a", 1)
            [| h |> PSObject.AsPSObject |]

        Pocof.buildProperties input inputObject |> shouldEqual expected
