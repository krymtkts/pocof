module pocof.Benchmark

open BenchmarkDotNet.Attributes

open System
open System.Collections
open System.Management.Automation

open Pocof
open Pocof.Data

[<MemoryDiagnoser>]
type PocofBenchmarks() =
    let psObjects = seq { 1..1000000 } |> Seq.map (string >> PSObject.AsPSObject)

    let hashtables =
        seq { 1..1000000 }
        |> Seq.map (fun i ->
            let h = new OrderedHashtable()
            h.Add("a", i)
            h |> Seq.cast<DictionaryEntry> |> Seq.head)

    let hashtablePsObjects =
        seq { 1..1000000 }
        |> Seq.map (fun i ->
            let h = new OrderedHashtable()
            h.Add("a", i)
            h)
        |> Seq.map PSObject.AsPSObject

    [<Benchmark>]
    member __.buildProperties_PSObject() =
        let properties: Pocof.PropertyStore = Pocof.PropertyStore()

        psObjects
        |> Seq.iter (Pocof.buildProperties properties.ContainsKey properties.Add)

    [<Benchmark>]
    member __.buildProperties_Hashtable() =
        let properties: Pocof.PropertyStore = Pocof.PropertyStore()

        hashtablePsObjects
        |> Seq.iter (Pocof.buildProperties properties.ContainsKey properties.Add)

    [<Benchmark>]
    member __.indexedProperty_PSObject() =
        psObjects |> Seq.iter (fun o -> o["Length"] |> ignore)

    [<Benchmark>]
    member __.indexedProperty_Hashtable() =
        hashtables |> Seq.iter (fun o -> o["Key"] |> ignore)

[<MemoryDiagnoser>]
type KeysBenchmarks() =
    let keyInfoA = [ new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false) ]

    let keyInfoAaa =
        [ new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false)
          new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false)
          new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false) ]

    let keyInfoShortcut =
        [ new ConsoleKeyInfo('\000', ConsoleKey.Escape, false, false, false) ]

    let keyInfoControl = [ new ConsoleKeyInfo('a', ConsoleKey.A, true, true, true) ]

    [<Benchmark>]
    member __.get_Char() =
        Keys.get Keys.defaultKeymap keyInfoA |> ignore

    [<Benchmark>]
    member __.get_Chars() =
        Keys.get Keys.defaultKeymap keyInfoAaa |> ignore

    [<Benchmark>]
    member __.get_ShortcutKey() =
        Keys.get Keys.defaultKeymap keyInfoShortcut |> ignore

    [<Benchmark>]
    member __.get_ControlKey() =
        Keys.get Keys.defaultKeymap keyInfoControl |> ignore

let state, context =
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
      Notification = None
      SuppressProperties = false
      Properties = []
      PropertyMap = Map []
      Prompt = "query>"
      PromptLength = 6
      WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
      ConsoleWidth = 0
      Refresh = Refresh.Required }
    |> InternalState.updateConsoleWidth 60
    |> Query.prepare

[<MemoryDiagnoser>]
type HandleBenchmarks() =
    [<Benchmark>]
    member __.invokeAction_Noop() =
        Action.Noop |> Handle.invokeAction state { Y = 0; Height = 20 } context

    [<Benchmark>]
    member __.invokeAction_AddQuery() =
        Action.AddQuery "a" |> Handle.invokeAction state { Y = 0; Height = 20 } context

    [<Benchmark>]
    member __.invokeAction_BackwardChar() =
        Action.BackwardChar |> Handle.invokeAction state { Y = 0; Height = 20 } context

    [<Benchmark>]
    member __.invokeAction_DeleteBackwardChar() =
        Action.DeleteBackwardChar
        |> Handle.invokeAction state { Y = 0; Height = 20 } context

    [<Benchmark>]
    member __.invokeAction_SelectBackwardChar() =
        Action.SelectBackwardChar
        |> Handle.invokeAction state { Y = 0; Height = 20 } context

    [<Benchmark>]
    member __.invokeAction_RotateMatcher() =
        Action.RotateMatcher |> Handle.invokeAction state { Y = 0; Height = 20 } context

    [<Benchmark>]
    member __.invokeAction_CompleteProperty() =
        Action.CompleteProperty
        |> Handle.invokeAction state { Y = 0; Height = 20 } context
