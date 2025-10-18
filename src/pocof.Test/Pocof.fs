module PocofTest.Pocof

open System
open System.Collections
open System.Management.Automation

open Expecto
open Expecto.Flip

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

[<Tests>]
let tests_initConsoleInterface =
    testList
        "initConsoleInterface"
        [

          test "When initializing console interface" {
              let actual = Pocof.initConsoleInterface ()

              actual.GetType()
              |> Expect.equal "should return ConsoleInterface type" typeof<Screen.ConsoleInterface>
          }

          ]

[<Tests>]
let tests_calculateWindowBeginningCursor =
    testList
        "calculateWindowBeginningCursor"
        [

          test "When query fits in window, cursor at end" {
              let state =
                  { Query = "a"
                    Cursor = 1
                    WindowBeginningCursor = 0
                    WindowWidth = 30
                    InputMode = InputMode.Input }

              let rui = new MockRawUI()
              use buff = new Screen.Buff(rui, (fun _ -> Seq.empty), Layout.TopDown, prompt)
              let actual = Pocof.calculateWindowBeginningCursor buff.GetLengthInBufferCells state

              actual
              |> Expect.equal "should return 0 when query fits in window and cursor at end" 0
          }

          test "When query overflows window, cursor at end" {
              let state =
                  { Query = String.replicate 31 "a"
                    Cursor = 31
                    WindowBeginningCursor = 0
                    WindowWidth = 30
                    InputMode = InputMode.Input }

              let rui = new MockRawUI()
              use buff = new Screen.Buff(rui, (fun _ -> Seq.empty), Layout.TopDown, prompt)
              let actual = Pocof.calculateWindowBeginningCursor buff.GetLengthInBufferCells state

              actual
              |> Expect.equal "should return 1 when query overflows window and cursor at end" 1
          }

          test "When window beginning cursor is set, cursor at start" {
              let state =
                  { Query = String.replicate 31 "a"
                    Cursor = 0
                    WindowBeginningCursor = 31
                    WindowWidth = 30
                    InputMode = InputMode.Input }

              let rui = new MockRawUI()
              use buff = new Screen.Buff(rui, (fun _ -> Seq.empty), Layout.TopDown, prompt)
              let actual = Pocof.calculateWindowBeginningCursor buff.GetLengthInBufferCells state

              actual
              |> Expect.equal "should return 0 when window beginning cursor is set and cursor at start" 0
          }

          ]

[<Tests>]
let tests_loop =
    testList
        "loop"
        [

          test "When finishing, should return result" {
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
              actual |> Expect.hasLength "should return 5 results when finishing" 5

              actual
              |> Seq.iteri (fun i x -> x |> Expect.equal (sprintf "should match result at index %d" i) results[i])

              rui.Check()
          }

          test "When canceling, shouldn't return result" {
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
              actual |> Expect.hasLength "should return 0 results when canceling" 0
              rui.Check()
          }

          test "When finishing after noop, should return result" {
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
              actual |> Expect.hasLength "should return 5 results after noop" 5

              actual
              |> Seq.iteri (fun i x ->
                  x
                  |> Expect.equal (sprintf "should match result at index %d after noop" i) results[i])

              rui.Check()
          }

          test "When finishing with filter, should return filtered result" {
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
              actual |> Expect.hasLength "should return 2 filtered results" 2

              Seq.item 0 actual |> Expect.equal "should match filtered result 0" results[0]

              Seq.item 1 actual |> Expect.equal "should match filtered result 1" results[3]

              rui.Check()
          }

          ]

[<Tests>]
let tests_interact =
    testList
        "interact"
        [

          test "When NonInteractive mode, should return result" {
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

              actual |> Expect.hasLength "should return 5 results in NonInteractive mode" 5

              actual
              |> List.ofSeq
              |> Expect.equal "should match expected results in NonInteractive mode" expected

              (buff :> IDisposable).Dispose()
          }

          test "When interaction finished in Interactive mode and TopDown Layout, should return result" {
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

              actual
              |> Expect.hasLength "should return 5 results in Interactive TopDown mode" 5

              actual
              |> List.ofSeq
              |> Expect.equal "should match expected results in Interactive TopDown mode" expected

              (buff :> IDisposable).Dispose()
          }

          test "When interaction finished in Interactive mode and BottomUp Layout, should return result" {
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

              actual
              |> Expect.hasLength "should return 5 results in Interactive BottomUp mode" 5

              actual
              |> List.ofSeq
              |> Expect.equal "should match expected results in Interactive BottomUp mode" expected

              (buff :> IDisposable).Dispose()
          }

          test "When interaction finished in Interactive mode and BottomUpHalf Layout, should return result" {
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

              actual
              |> Expect.hasLength "should return 5 results in Interactive BottomUpHalf mode" 5

              actual
              |> List.ofSeq
              |> Expect.equal "should match expected results in Interactive BottomUpHalf mode" expected

              (buff :> IDisposable).Dispose()
          }

          ]

[<Tests>]
let tests_initScreen =
    // NOTE: covered by interact tests.
    testList "initScreen" []

open System.Threading

[<Tests>]
let tests_render =
    testList
        "render"
        [

          test "When handler has a quit event, should return ContinueProcessing.StopUpstreamCommands" {
              let config: InternalConfig =
                  { NotInteractive = false
                    Layout = Layout.BottomUpHalf
                    Keymaps = Keys.defaultKeymap
                    WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                    Prompt = prompt
                    PromptLength = prompt |> String.length
                    Properties = []
                    PropertiesMap = Map [] }

              let handler = new Pocof.RenderHandler()

              async {
                  Thread.Sleep 300

                  (state, lazy PSeq.empty, lazy Error "error")
                  |> Pocof.RenderEvent.Render
                  |> handler.Publish

                  Thread.Sleep 300

                  (state, lazy PSeq.empty, lazy Ok(seq { "Value" }))
                  |> Pocof.RenderEvent.Render
                  |> handler.Publish

                  (state, lazy PSeq.empty, lazy Ok(seq { "Value" }))
                  |> Pocof.RenderEvent.Render
                  |> handler.Publish

                  Thread.Sleep 300

                  Pocof.RenderEvent.Quit |> handler.Publish

                  (state, lazy PSeq.empty, lazy Ok(seq { "Value" }))
                  |> Pocof.RenderEvent.Render
                  |> handler.Publish
              }
              |> Async.Start

              let rui = new MockRawUI()
              let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt

              // TODO: check if the rendering finished expectedly.
              Pocof.render buff handler

          }

          ]

type MockException(o: obj) =
    inherit Exception()

type MockCmdlet() =
    inherit PSCmdlet()

[<Tests>]
let tests_stopUpstreamCommandsException =
    testList
        "stopUpstreamCommandsException"
        [

          test "When called, should return the exception instance" {
              let actual =
                  Pocof.stopUpstreamCommandsException typeof<MockException> (new MockCmdlet())

              actual.GetType()
              |> Expect.equal "should return MockException type" typeof<MockException>
          }

          ]

[<Tests>]
let tests_renderOnce =
    testList
        "renderOnce"
        [

          test "When handler is empty, should return ContinueProcessing.Continue" {
              let config: InternalConfig =
                  { NotInteractive = false
                    Layout = Layout.BottomUpHalf
                    Keymaps = Keys.defaultKeymap
                    WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                    Prompt = prompt
                    PromptLength = prompt |> String.length
                    Properties = []
                    PropertiesMap = Map [] }

              let handler = new Pocof.RenderHandler()
              let rui = new MockRawUI()
              let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
              let actual = Pocof.renderOnce handler buff

              actual
              |> function
                  | Pocof.RenderProcess.Noop -> true
                  | _ -> false
              |> Expect.isTrue "should return ContinueProcessing.Continue when handler is empty"
          }

          test "When handler has a render event, should return ContinueProcessing.Continue" {
              let config: InternalConfig =
                  { NotInteractive = false
                    Layout = Layout.BottomUpHalf
                    Keymaps = Keys.defaultKeymap
                    WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                    Prompt = prompt
                    PromptLength = prompt |> String.length
                    Properties = []
                    PropertiesMap = Map [] }

              let handler = new Pocof.RenderHandler()

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
              |> Expect.isTrue "should return ContinueProcessing.Continue when handler has a render event"
          }

          test "When handler has a quit event, should return ContinueProcessing.StopUpstreamCommands" {
              let config: InternalConfig =
                  { NotInteractive = false
                    Layout = Layout.BottomUpHalf
                    Keymaps = Keys.defaultKeymap
                    WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                    Prompt = prompt
                    PromptLength = prompt |> String.length
                    Properties = []
                    PropertiesMap = Map [] }

              let handler = new Pocof.RenderHandler()
              Pocof.RenderEvent.Quit |> handler.Publish
              let rui = new MockRawUI()
              let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
              let actual = Pocof.renderOnce handler buff

              actual
              |> function
                  | Pocof.RenderProcess.StopUpstreamCommands -> true
                  | _ -> false
              |> Expect.isTrue "should return ContinueProcessing.StopUpstreamCommands when handler has a quit event"
          }

          ]

[<Tests>]
let tests_Periodic =
    testList
        "Periodic"
        [

          test "When stopping upstream commands, should invoke cancel action" {
              let config: InternalConfig =
                  { NotInteractive = false
                    Layout = Layout.BottomUpHalf
                    Keymaps = Keys.defaultKeymap
                    WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                    Prompt = prompt
                    PromptLength = prompt |> String.length
                    Properties = []
                    PropertiesMap = Map [] }

              let handler = new Pocof.RenderHandler()
              Pocof.RenderEvent.Quit |> handler.Publish
              let rui = new MockRawUI()
              let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
              let mutable actual = false

              let periodic =
                  Pocof.Periodic(handler, buff, (fun _ -> actual <- true), config.PromptLength)

              Thread.Sleep 100
              periodic.Render()

              actual
              |> Expect.isTrue "should invoke cancel action when stopping upstream commands"

              periodic.Stop()
          }

          test "When rendering completed, shouldn't invoke cancel action" {
              let config: InternalConfig =
                  { NotInteractive = false
                    Layout = Layout.BottomUpHalf
                    Keymaps = Keys.defaultKeymap
                    WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                    Prompt = prompt
                    PromptLength = prompt |> String.length
                    Properties = []
                    PropertiesMap = Map [] }

              let handler = new Pocof.RenderHandler()
              Pocof.RenderEvent.Render(state, lazy PSeq.empty, lazy Ok []) |> handler.Publish
              let rui = new MockRawUI()
              let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
              let mutable actual = false

              let periodic =
                  Pocof.Periodic(handler, buff, (fun _ -> actual <- true), config.PromptLength)

              Thread.Sleep 100
              periodic.Render()

              actual |> Expect.isFalse "shouldn't invoke cancel action if rendering completed"

              periodic.Stop()
          }

          test "When ElapsedMilliseconds is less than 10ms, shouldn't invoke anything" {
              let config: InternalConfig =
                  { NotInteractive = false
                    Layout = Layout.BottomUpHalf
                    Keymaps = Keys.defaultKeymap
                    WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                    Prompt = prompt
                    PromptLength = prompt |> String.length
                    Properties = []
                    PropertiesMap = Map [] }

              let handler = new Pocof.RenderHandler()
              let rui = new MockRawUI()
              let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
              let mutable actual = false

              let periodic =
                  Pocof.Periodic(handler, buff, (fun _ -> actual <- true), config.PromptLength)

              periodic.Render()

              actual
              |> Expect.isFalse "shouldn't invoke anything if ElapsedMilliseconds is less than 10ms"

              periodic.Stop()
          }

          test "When first time idle rendering, shouldn't invoke idling action" {
              let config: InternalConfig =
                  { NotInteractive = false
                    Layout = Layout.BottomUpHalf
                    Keymaps = Keys.defaultKeymap
                    WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                    Prompt = prompt
                    PromptLength = prompt |> String.length
                    Properties = []
                    PropertiesMap = Map [] }

              let handler = new Pocof.RenderHandler()
              let rui = new MockRawUI()
              let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
              let mutable actual = false

              let periodic =
                  Pocof.Periodic(handler, buff, (fun _ -> actual <- true), config.PromptLength)

              Thread.Sleep 100
              periodic.Render()

              actual
              |> Expect.isFalse "shouldn't invoke idling action if first time idle rendering"

              periodic.Stop()
          }

          test "When idling for 1 second, should invoke idling action" {
              let config: InternalConfig =
                  { NotInteractive = false
                    Layout = Layout.BottomUpHalf
                    Keymaps = Keys.defaultKeymap
                    WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                    Prompt = prompt
                    PromptLength = prompt |> String.length
                    Properties = []
                    PropertiesMap = Map [] }

              let handler = new Pocof.RenderHandler()
              let rui = new MockRawUI()
              let buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
              let mutable actual = false

              let periodic =
                  Pocof.Periodic(handler, buff, (fun _ -> actual <- true), config.PromptLength)

              Thread.Sleep 1000
              periodic.Render()
              actual |> Expect.isFalse "should invoke idling action after 1 second"
              periodic.Stop()
          }

          test "When idling rendering for 1 second, should invoke idling rendering action" {
              let config: InternalConfig =
                  { NotInteractive = false
                    Layout = Layout.BottomUpHalf
                    Keymaps = Keys.defaultKeymap
                    WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                    Prompt = prompt
                    PromptLength = prompt |> String.length
                    Properties = []
                    PropertiesMap = Map [] }

              let handler = new Pocof.RenderHandler()
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

              actual |> Expect.isFalse "should invoke idling rendering action after 1 second"

              periodic.Stop()
          }

          ]

[<Tests>]
let tests_NormalInputStore =
    let mapToObj x =
        x |> List.map (PSObject.AsPSObject >> Entry.Obj)

    testList
        "NormalInputStore"
        [

          test "When adding one Obj, should return the list with added Obj" {
              let expected = [ 1 ] |> mapToObj
              let input: Pocof.IInputStore = Pocof.getInputStore false
              input.Add(1 |> PSObject.AsPSObject)

              input.GetEntries()
              |> List.ofSeq
              |> Expect.equal "should return the list with added Obj" expected
          }
          test "When adding two Objs, should return the list with added Obj to tail" {
              let expected = [ 0; 1 ] |> mapToObj
              let input: Pocof.IInputStore = Pocof.getInputStore false
              input.Add(0 |> PSObject.AsPSObject)
              input.Add(1 |> PSObject.AsPSObject)

              input.GetEntries()
              |> List.ofSeq
              |> Expect.equal "should return the list with added Obj to tail" expected
          }
          test "When adding Dict, should return the list with added Dict" {
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

              input.GetEntries()
              |> List.ofSeq
              |> Expect.equal "should return the list with added Dict" expected

              input.Count()
              |> Expect.equal "should return correct count for Dict" (Seq.length expected)
          }

          ]

[<Tests>]
let tests_UniqueInputStore =
    let mapToObj x =
        x |> List.map (PSObject.AsPSObject >> Entry.Obj)

    testList
        "UniqueInputStore"
        [

          test "When adding one Obj, should return the list with added Obj" {
              let expected = [ 1 ] |> mapToObj
              let input: Pocof.IInputStore = Pocof.getInputStore true
              input.Add(1 |> PSObject.AsPSObject)

              input.GetEntries()
              |> List.ofSeq
              |> Expect.equal "should return the list with added Obj" expected
          }

          test "When adding two Objs, should return the list with added Obj to tail" {
              let expected = [ 0; 1 ] |> mapToObj
              let input: Pocof.IInputStore = Pocof.getInputStore true
              input.Add(0 |> PSObject.AsPSObject)
              input.Add(1 |> PSObject.AsPSObject)

              input.GetEntries()
              |> List.ofSeq
              |> Expect.equal "should return the list with added Obj to tail" expected
          }

          test "When adding Dict, should return the list with added Dict" {
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

              input.GetEntries()
              |> List.ofSeq
              |> Expect.equal "should return the list with added Dict" expected
          }

          test "When adding duplicate Objs, should return the unique list" {
              let expected = [ 1; 2; 3 ] |> mapToObj
              let input: Pocof.IInputStore = Pocof.getInputStore true
              input.Add(1 |> PSObject.AsPSObject)
              input.Add(2 |> PSObject.AsPSObject)
              input.Add(3 |> PSObject.AsPSObject)
              input.Add(2 |> PSObject.AsPSObject)
              input.Add(1 |> PSObject.AsPSObject)

              input.GetEntries()
              |> List.ofSeq
              |> Expect.equal "should return the unique list" expected

              input.Count()
              |> Expect.equal "should return correct count for unique list" (Seq.length expected)
          }

          test "When adding duplicate Dicts, should return the unique list with Dict" {
              let expected =
                  [ DictionaryEntry("a", 1)
                    DictionaryEntry("b", 2)
                    DictionaryEntry("c", 3)
                    DictionaryEntry("b", 4) ]
                  |> List.map Entry.Dict

              let input: Pocof.IInputStore = Pocof.getInputStore true
              let h1 = new OrderedHashtable()
              h1.Add("a", 1)
              h1.Add("b", 2)
              h1.Add("c", 3)
              h1 |> PSObject.AsPSObject |> input.Add
              let h2 = new OrderedHashtable()
              h2.Add("a", 1)
              h2.Add("b", 4)
              h2.Add("c", 3)
              h2 |> PSObject.AsPSObject |> input.Add

              input.GetEntries()
              |> List.ofSeq
              |> Expect.equal "should return the unique list with Dict" expected

              input.Count()
              |> Expect.equal "should return correct count for unique Dict list" (Seq.length expected)
          }

          ]

[<Tests>]
let tests_buildProperties =
    testList
        "buildProperties"
        [

          test "When adding input properties, should return the list with added input properties" {
              let expected = [ "a"; "b"; "c" ]
              let props: Generic.Dictionary<string, string seq> = Generic.Dictionary()

              let inputObject =
                  let o = new PSObject()
                  o.Properties.Add(new PSNoteProperty("a", 1))
                  o.Properties.Add(new PSNoteProperty("b", 2))
                  o.Properties.Add(new PSNoteProperty("c", 3))
                  o

              Pocof.buildProperties props.ContainsKey props.Add inputObject

              props.Values
              |> Seq.concat
              |> List.ofSeq
              |> Expect.equal "should return the list with added input properties" expected
          }

          test
              "When adding input properties with duplication, should return the list with added input properties with duplication" {
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

              props.Values
              |> Seq.concat
              |> List.ofSeq
              |> Expect.equal "should return the list with added input properties with duplication" expected
          }

          test "When input type already exists, should return the same list" {
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

              props.Values
              |> Seq.concat
              |> List.ofSeq
              |> Expect.equal "should return the same list when input type already exists" expected
          }

          test "When adding hashtable, should return the list with added the keys of hashtable" {
              let expected = [ "Key"; "Value" ]
              let props: Generic.Dictionary<string, string seq> = Generic.Dictionary()

              let inputObject =
                  let h = new OrderedHashtable()
                  h.Add("a", 1)
                  h.Add("b", 2)
                  h.Add("c", 3)
                  h |> PSObject.AsPSObject

              Pocof.buildProperties props.ContainsKey props.Add inputObject

              props.Values
              |> Seq.concat
              |> List.ofSeq
              |> Expect.equal "should return the list with added the keys of hashtable" expected
          }

          test "When adding empty hashtable, should return the empty list" {
              let expected = []
              let props: Generic.Dictionary<string, string seq> = Generic.Dictionary()

              let inputObject =
                  let h = new OrderedHashtable()
                  h |> PSObject.AsPSObject

              Pocof.buildProperties props.ContainsKey props.Add inputObject

              props.Values
              |> Seq.concat
              |> List.ofSeq
              |> Expect.equal "should return the empty list when empty hashtable" expected
          }

          ]

[<Tests>]
let tests_PropertyStore =
    testList
        "PropertyStore"
        [

          test "When adding values, should return the added values" {
              let expected = [ "a"; "b"; "c" ]
              let properties: Pocof.PropertyStore = Pocof.PropertyStore()
              properties.Add("a", [ "a"; "b"; "c" ])

              properties.GetProperties()
              |> List.ofSeq
              |> Expect.equal "should return the added values" expected
          }

          test "When adding existing name, shouldn't add new values" {
              let expected = [ "a"; "b"; "c" ]
              let properties: Pocof.PropertyStore = Pocof.PropertyStore()
              properties.Add("a", [ "a"; "b"; "c" ])
              properties.Add("a", [ "d"; "e"; "f" ])

              properties.GetProperties()
              |> List.ofSeq
              |> Expect.equal "should not add values for existing name" expected
          }

          test "When adding duplicated values, shouldn't add duplicates" {
              let expected = [ "a"; "b"; "c" ]
              let properties: Pocof.PropertyStore = Pocof.PropertyStore()
              properties.Add("a", [ "a"; "b"; "a"; "b"; "c" ])

              properties.GetProperties()
              |> List.ofSeq
              |> Expect.equal "should not add duplicated values" expected
          }

          ]
