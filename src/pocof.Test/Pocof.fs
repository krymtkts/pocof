module PocofTest.Pocof

open System
open System.Management.Automation

open Xunit
open FsUnitTyped

open Pocof
open Pocof.Data
open Pocof.Test

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
      SuppressProperties = false
      Refresh = Refresh.Required }

let prompt = "query>"
let state = initState ()
let publishEvent _ = ()
let results = [ "a"; "b"; "c"; "d"; "e" ] |> List.map box

module initConsoleInterface =
    [<Fact>]
    let ``should return ConsoleInterface.`` () =
        let actual = Pocof.initConsoleInterface ()
        actual.GetType() |> shouldEqual typeof<Screen.ConsoleInterface>

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
        use buff = new Screen.Buff(rui, (fun _ -> Seq.empty), Layout.TopDown, prompt)
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
        use buff = new Screen.Buff(rui, (fun _ -> Seq.empty), Layout.TopDown, prompt)
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
        use buff = new Screen.Buff(rui, (fun _ -> Seq.empty), Layout.TopDown, prompt)
        let actual = Pocof.calculateWindowBeginningCursor buff.GetLengthInBufferCells state

        actual |> shouldEqual 0

module loop =

    [<Fact>]
    let ``should return result when finishing.`` () =
        let input = results |> List.map toObj
        let rui = new MockRawUI(60, 30, [ MockRawUI.ConsoleKey '\000' ConsoleKey.Enter ])

        let config: InternalConfig =
            { NotInteractive = true
              Layout = Layout.TopDown
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let actual = Pocof.interact config state buff publishEvent input

        actual |> Seq.length |> shouldEqual 5
        actual |> Seq.iteri (fun i x -> x = results.[i] |> shouldEqual true)
        rui.Check()

    [<Fact>]
    let ``shouldn't return result when canceling.`` () =
        let input = results |> List.map toObj
        let rui = new MockRawUI(60, 30, [], true)

        let config: InternalConfig =
            { NotInteractive = true
              Layout = Layout.TopDown
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let actual = Pocof.interact config state buff publishEvent input

        actual |> Seq.length |> shouldEqual 0
        rui.Check()

    [<Fact>]
    let ``should return result when finishing after noop.`` () =
        let input = results |> List.map toObj

        let rui =
            new MockRawUI(
                60,
                30,
                [ new ConsoleKeyInfo('\000', ConsoleKey.Escape, true, true, false) |> Some
                  None
                  MockRawUI.ConsoleKey '\000' ConsoleKey.Enter ]
            )

        let config: InternalConfig =
            { NotInteractive = true
              Layout = Layout.TopDown
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let actual = Pocof.interact config state buff publishEvent input

        actual |> Seq.length |> shouldEqual 5
        actual |> Seq.iteri (fun i x -> x = results.[i] |> shouldEqual true)
        rui.Check()

    [<Fact>]
    let ``should return result when finishing with filter.`` () =
        let input = results |> List.map toObj

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

        let config: InternalConfig =
            { NotInteractive = true
              Layout = Layout.TopDown
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let actual = Pocof.interact config state buff publishEvent input

        actual |> Seq.length |> shouldEqual 2
        Seq.item 0 actual = results.[0] |> shouldEqual true
        Seq.item 1 actual = results.[3] |> shouldEqual true
        rui.Check()

module interact =
    [<Fact>]
    let ``should return result with NonInteractive mode.`` () =
        let config: InternalConfig =
            { NotInteractive = true
              Layout = Layout.TopDown
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let input = results |> List.map toObj
        let rui = new MockRawUI()
        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let actual = Pocof.interact config state buff publishEvent input
        let expected = [ "a"; "b"; "c"; "d"; "e" ] |> List.map (PSObject.AsPSObject >> box)

        actual |> Seq.length |> shouldEqual 5
        actual |> List.ofSeq |> shouldEqual expected
        (buff :> IDisposable).Dispose()

    [<Fact>]
    let ``should return result when interaction finished in Interactive mode and TopDown Layout.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.TopDown
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let input = results |> List.map toObj
        let rui = new MockRawUI()
        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let actual = Pocof.interact config state buff publishEvent input
        let expected = [ "a"; "b"; "c"; "d"; "e" ] |> List.map (PSObject.AsPSObject >> box)

        actual |> Seq.length |> shouldEqual 5
        actual |> List.ofSeq |> shouldEqual expected
        (buff :> IDisposable).Dispose()

    [<Fact>]
    let ``should return result when interaction finished in Interactive mode and BottomUp Layout.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUp
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let input = results |> List.map toObj
        let rui = new MockRawUI()
        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let actual = Pocof.interact config state buff publishEvent input
        let expected = [ "a"; "b"; "c"; "d"; "e" ] |> List.map (PSObject.AsPSObject >> box)

        actual |> Seq.length |> shouldEqual 5
        actual |> List.ofSeq |> shouldEqual expected
        (buff :> IDisposable).Dispose()

    [<Fact>]
    let ``should return result when interaction finished in Interactive mode and BottomUpHalp Layout.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUpHalf
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let input = results |> List.map toObj
        let rui = new MockRawUI()
        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let actual = Pocof.interact config state buff publishEvent input
        let expected = [ "a"; "b"; "c"; "d"; "e" ] |> List.map (PSObject.AsPSObject >> box)

        actual |> Seq.length |> shouldEqual 5
        actual |> List.ofSeq |> shouldEqual expected
        (buff :> IDisposable).Dispose()

module initScreen =
    // NOTE: covered by interact tests.
    ()

module render =
    open System.Threading

    [<Fact>]
    let ``should return ContinueProcessing.StopUpstreamCommands when handler has a quit event.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUpHalf
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let handler = Pocof.RenderHandler()

        async {
            Thread.Sleep 300

            (state, lazy PSeq.empty, lazy Error "error")
            |> Pocof.RenderEvent.Render
            |> handler.Publish

            Thread.Sleep 300

            (state, lazy PSeq.empty, lazy Ok [ "Value" ])
            |> Pocof.RenderEvent.Render
            |> handler.Publish

            (state, lazy PSeq.empty, lazy Ok [ "Value" ])
            |> Pocof.RenderEvent.Render
            |> handler.Publish

            Thread.Sleep 300

            Pocof.RenderEvent.Quit |> handler.Publish

            (state, lazy PSeq.empty, lazy Ok [ "Value" ])
            |> Pocof.RenderEvent.Render
            |> handler.Publish
        }
        |> Async.Start

        let rui = new MockRawUI()
        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let actual = Pocof.render buff handler
        actual |> shouldEqual ()

module stopUpstreamCommandsException =
    type MockException(o: obj) =
        inherit Exception()

    type MockCmdlet() =
        inherit PSCmdlet()

    [<Fact>]
    let ``should return the exception instance.`` () =
        let actual =
            Pocof.stopUpstreamCommandsException typeof<MockException> (new MockCmdlet())

        actual.GetType() |> shouldEqual typeof<MockException>

module renderOnce =
    [<Fact>]
    let ``should return ContinueProcessing.Continue when handler is empty.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUpHalf
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let handler = Pocof.RenderHandler()

        let rui = new MockRawUI()
        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let actual = Pocof.renderOnce handler buff

        actual
        |> function
            | Pocof.RenderProcess.Noop -> true
            | _ -> false
        |> shouldEqual true

    [<Fact>]
    let ``should return ContinueProcessing.Continue when handler has a render event.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUpHalf
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let handler = Pocof.RenderHandler()

        (state, lazy PSeq.empty, lazy Error "error")
        |> Pocof.RenderEvent.Render
        |> handler.Publish

        let rui = new MockRawUI()
        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let actual = Pocof.renderOnce handler buff

        actual
        |> function
            | Pocof.RenderProcess.Rendered _ -> true
            | _ -> false
        |> shouldEqual true

    [<Fact>]
    let ``should return ContinueProcessing.StopUpstreamCommands when handler has a quit event.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUpHalf
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let handler = Pocof.RenderHandler()
        Pocof.RenderEvent.Quit |> handler.Publish
        let rui = new MockRawUI()
        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let actual = Pocof.renderOnce handler buff

        actual |> shouldEqual Pocof.RenderProcess.StopUpstreamCommands

module Interval =
    open System.Threading

    [<Fact>]
    let ``should invoke cancel action when stopping upstream commands.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUpHalf
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let handler = Pocof.RenderHandler()
        Pocof.RenderEvent.Quit |> handler.Publish
        let rui = new MockRawUI()
        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let mutable actual = false

        let periodic =
            Pocof.Periodic(handler, buff, (fun _ -> actual <- true), config.PromptLength)

        Thread.Sleep 100
        periodic.Render()
        actual |> shouldEqual true
        periodic.Stop()

    [<Fact>]
    let ``shouldn't invoke cancel action if rendering completed.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUpHalf
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let handler = Pocof.RenderHandler()
        Pocof.RenderEvent.Render(state, lazy PSeq.empty, lazy Ok []) |> handler.Publish
        let rui = new MockRawUI()
        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let mutable actual = false

        let periodic =
            Pocof.Periodic(handler, buff, (fun _ -> actual <- true), config.PromptLength)

        Thread.Sleep 100
        periodic.Render()
        actual |> shouldEqual false
        periodic.Stop()

    [<Fact>]
    let ``shouldn't invoke anything if ElapsedMilliseconds is less than 10ms.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUpHalf
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let handler = Pocof.RenderHandler()
        let rui = new MockRawUI()
        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let mutable actual = false

        let periodic =
            Pocof.Periodic(handler, buff, (fun _ -> actual <- true), config.PromptLength)

        periodic.Render()
        actual |> shouldEqual false
        periodic.Stop()

    [<Fact>]
    let ``shouldn't invoke idling action if first time idle rendering.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUpHalf
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let handler = Pocof.RenderHandler()
        let rui = new MockRawUI()
        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let mutable actual = false

        let periodic =
            Pocof.Periodic(handler, buff, (fun _ -> actual <- true), config.PromptLength)

        Thread.Sleep 100
        periodic.Render()
        actual |> shouldEqual false
        periodic.Stop()

    [<Fact>]
    let ``should invoke idling action after 1 second.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUpHalf
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let handler = Pocof.RenderHandler()
        let rui = new MockRawUI()
        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let mutable actual = false

        let periodic =
            Pocof.Periodic(handler, buff, (fun _ -> actual <- true), config.PromptLength)

        Thread.Sleep 1000
        periodic.Render()
        actual |> shouldEqual false
        periodic.Stop()

    [<Fact>]
    let ``should invoke idling rendering action after 1 second.`` () =
        let config: InternalConfig =
            { NotInteractive = false
              Layout = Layout.BottomUpHalf
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        let handler = Pocof.RenderHandler()
        Pocof.RenderEvent.Render(state, lazy PSeq.empty, lazy Ok []) |> handler.Publish
        let rui = new MockRawUI()
        let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        let mutable actual = false

        let periodic =
            Pocof.Periodic(handler, buff, (fun _ -> actual <- true), config.PromptLength)

        Thread.Sleep 100

        List.replicate 12 ()
        |> List.iter (fun _ ->
            periodic.Render()
            Thread.Sleep 100)

        actual |> shouldEqual false
        periodic.Stop()

module NormalInputStore =
    open System.Collections

    let mapToObj x =
        x |> List.map (PSObject.AsPSObject >> Entry.Obj)

    [<Fact>]
    let ``should return the list with added Obj`` () =
        let expected = [ 1 ] |> mapToObj
        let input: Pocof.IInputStore = Pocof.getInputStore false
        input.Add(1 |> PSObject.AsPSObject)
        input.GetEntries() |> List.ofSeq |> shouldEqual expected

    [<Fact>]
    let ``should return the list with added Obj to tail.`` () =
        let expected = [ 0; 1 ] |> mapToObj
        let input: Pocof.IInputStore = Pocof.getInputStore false
        input.Add(0 |> PSObject.AsPSObject)
        input.Add(1 |> PSObject.AsPSObject)
        input.GetEntries() |> List.ofSeq |> shouldEqual expected

    [<Fact>]
    let ``should return the list with added Dict`` () =
        let expected =
            [ DictionaryEntry("a", 1); DictionaryEntry("b", 2); DictionaryEntry("c", 3) ]
            |> List.map Entry.Dict

        let input: Pocof.IInputStore = Pocof.getInputStore false

        let inputObject =
            let h = new OrderedHashtable()
            h.Add("a", 1)
            h.Add("b", 2)
            h.Add("c", 3)
            h |> PSObject.AsPSObject

        input.Add inputObject
        input.GetEntries() |> List.ofSeq |> shouldEqual expected
        input.Count() |> shouldEqual (Seq.length expected)

module UniqueInputStore =
    open System.Collections

    let mapToObj x =
        x |> List.map (PSObject.AsPSObject >> Entry.Obj)

    [<Fact>]
    let ``should return the list with added Obj`` () =
        let expected = [ 1 ] |> mapToObj
        let input: Pocof.IInputStore = Pocof.getInputStore true
        input.Add(1 |> PSObject.AsPSObject)
        input.GetEntries() |> List.ofSeq |> shouldEqual expected

    [<Fact>]
    let ``should return the list with added Obj to tail.`` () =
        let expected = [ 0; 1 ] |> mapToObj
        let input: Pocof.IInputStore = Pocof.getInputStore true
        input.Add(0 |> PSObject.AsPSObject)
        input.Add(1 |> PSObject.AsPSObject)
        input.GetEntries() |> List.ofSeq |> shouldEqual expected

    [<Fact>]
    let ``should return the list with added Dict`` () =
        let expected =
            [ DictionaryEntry("a", 1); DictionaryEntry("b", 2); DictionaryEntry("c", 3) ]
            |> List.map Entry.Dict

        let input: Pocof.IInputStore = Pocof.getInputStore true

        let inputObject =
            let h = new OrderedHashtable()
            h.Add("a", 1)
            h.Add("b", 2)
            h.Add("c", 3)
            h |> PSObject.AsPSObject

        input.Add inputObject
        input.GetEntries() |> List.ofSeq |> shouldEqual expected

    [<Fact>]
    let ``should return the unique list.`` () =
        let expected = [ 1; 2; 3 ] |> mapToObj
        let input: Pocof.IInputStore = Pocof.getInputStore true

        input.Add(1 |> PSObject.AsPSObject)
        input.Add(2 |> PSObject.AsPSObject)
        input.Add(3 |> PSObject.AsPSObject)
        input.Add(2 |> PSObject.AsPSObject)
        input.Add(1 |> PSObject.AsPSObject)
        input.GetEntries() |> List.ofSeq |> shouldEqual expected
        input.Count() |> shouldEqual (Seq.length expected)

    [<Fact>]
    let ``should return the unique list with Dict.`` () =
        let expected =
            [ DictionaryEntry("a", 1)
              DictionaryEntry("b", 2)
              DictionaryEntry("c", 3)
              DictionaryEntry("b", 4) ]
            |> List.map Entry.Dict

        let input: Pocof.IInputStore = Pocof.getInputStore true

        let h = new OrderedHashtable()
        h.Add("a", 1)
        h.Add("b", 2)
        h.Add("c", 3)
        h |> PSObject.AsPSObject |> input.Add

        let h = new OrderedHashtable()
        h.Add("a", 1)
        h.Add("b", 4)
        h.Add("c", 3)
        h |> PSObject.AsPSObject |> input.Add

        input.GetEntries() |> List.ofSeq |> shouldEqual expected
        input.Count() |> shouldEqual (Seq.length expected)

module buildProperties =
    open System.Collections

    [<Fact>]
    let ``should return the list with added input properties.`` () =
        let expected = [ "a"; "b"; "c" ]
        let props: Generic.Dictionary<string, string seq> = Generic.Dictionary()

        let inputObject =
            let o = new PSObject()
            o.Properties.Add(new PSNoteProperty("a", 1))
            o.Properties.Add(new PSNoteProperty("b", 2))
            o.Properties.Add(new PSNoteProperty("c", 3))
            o

        Pocof.buildProperties props.ContainsKey props.Add inputObject
        props.Values |> Seq.concat |> List.ofSeq |> shouldEqual expected

    [<Fact>]
    let ``should return the list with added input properties with duplication.`` () =
        let expected = [ "a"; "a"; "b"; "c" ]
        let props: Generic.Dictionary<string, string seq> = Generic.Dictionary()
        props.Add("SomeObject", [ "a" ])

        let inputObject =
            let o = new PSObject()
            o.Properties.Add(new PSNoteProperty("a", 1))
            o.Properties.Add(new PSNoteProperty("b", 2))
            o.Properties.Add(new PSNoteProperty("c", 3))
            o

        Pocof.buildProperties props.ContainsKey props.Add inputObject
        props.Values |> Seq.concat |> List.ofSeq |> shouldEqual expected

    [<Fact>]
    let ``should return the same list when input type already exists.`` () =
        let expected = [ "a"; "b"; "c" ]
        let props: Generic.Dictionary<string, string seq> = Generic.Dictionary()
        let o = new PSObject()
        props.Add((nullArgCheck "str" (o.BaseObject.GetType().FullName)), [ "a"; "b"; "c" ])

        let inputObject =
            let o = new PSObject()
            o.Properties.Add(new PSNoteProperty("aa", 1))
            o.Properties.Add(new PSNoteProperty("bb", 2))
            o.Properties.Add(new PSNoteProperty("cc", 3))
            o

        Pocof.buildProperties props.ContainsKey props.Add inputObject
        props.Values |> Seq.concat |> List.ofSeq |> shouldEqual expected

    [<Fact>]
    let ``should return the list with added the keys of hashtable.`` () =
        let expected = [ "Key"; "Value" ]
        let props: Generic.Dictionary<string, string seq> = Generic.Dictionary()

        let inputObject =
            let h = new OrderedHashtable()
            h.Add("a", 1)
            h.Add("b", 2)
            h.Add("c", 3)
            h |> PSObject.AsPSObject

        Pocof.buildProperties props.ContainsKey props.Add inputObject
        props.Values |> Seq.concat |> List.ofSeq |> shouldEqual expected

    [<Fact>]
    let ``should return the empty list when empty hashtable.`` () =
        let expected = []
        let props: Generic.Dictionary<string, string seq> = Generic.Dictionary()

        let inputObject =
            let h = new OrderedHashtable()
            h |> PSObject.AsPSObject

        Pocof.buildProperties props.ContainsKey props.Add inputObject
        props.Values |> Seq.concat |> List.ofSeq |> shouldEqual expected

module PropertyStore =
    [<Fact>]
    let ``should add values.`` () =
        let expected = [ "a"; "b"; "c" ]
        let properties: Pocof.PropertyStore = Pocof.PropertyStore()
        properties.Add("a", [ "a"; "b"; "c" ])
        properties.GetProperties() |> List.ofSeq |> shouldEqual expected

    [<Fact>]
    let ``shouldn't add existing name.`` () =
        let expected = [ "a"; "b"; "c" ]
        let properties: Pocof.PropertyStore = Pocof.PropertyStore()
        properties.Add("a", [ "a"; "b"; "c" ])
        properties.Add("a", [ "d"; "e"; "f" ])
        properties.GetProperties() |> List.ofSeq |> shouldEqual expected

    [<Fact>]
    let ``shouldn't add duplicated values.`` () =
        let expected = [ "a"; "b"; "c" ]
        let properties: Pocof.PropertyStore = Pocof.PropertyStore()
        properties.Add("a", [ "a"; "b"; "a"; "b"; "c" ])
        properties.GetProperties() |> List.ofSeq |> shouldEqual expected
