module pocof.Benchmark

open BenchmarkDotNet.Attributes

open System
open System.Collections
open System.Collections.Concurrent
open System.Management.Automation

open Pocof
open Pocof.Data
open Pocof.Test
open System.Linq
open Pocof.Keys

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
            h |> PSObject.AsPSObject)

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
    let keyInfoA =
        KeyBatch([| new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false) |], 1)

    let keyInfoAaa =
        KeyBatch(
            [| new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false)
               new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false)
               new ConsoleKeyInfo('a', ConsoleKey.A, false, false, false) |],
            3
        )

    let keyInfoShortcut =
        KeyBatch([| new ConsoleKeyInfo('\000', ConsoleKey.Escape, false, false, false) |], 1)

    let keyInfoControl =
        KeyBatch([| new ConsoleKeyInfo('a', ConsoleKey.A, true, true, true) |], 1)

    let keymaps =
        let h = new Hashtable()

        [ ("tab", "CompleteProperty")
          ("alt+a", "DeleteBackwardChar")
          ("ctrl+d", "SelectBackwardChar")
          ("ctrl+shift+e", "RotateMatcher")
          ("control+alt+shift+x", "cancel") ]
        |> List.map h.Add
        |> ignore

        h

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

    [<Benchmark>]
    member __.convertKeymaps() = Keys.convertKeymaps keymaps |> ignore

[<MemoryDiagnoser>]
type HandleBenchmarks() =
    let state =
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
          Refresh = Refresh.Required
          QueryCache = ValueNone }
        |> InternalState.updateConsoleWidth ("query>" |> String.length) 60

    let stateForWord =
        { state with
            InternalState.QueryState.Query = ":Name foo :Value bar"
            InternalState.QueryState.Cursor = 8 }

    let stateForPropertySearch =
        { state with
            InternalState.QueryState.Query = ":Name"
            InternalState.QueryState.Cursor = 3
            PropertySearch = PropertySearch.Search("Na") }

    let stateForPropertyRotate =
        { state with
            InternalState.QueryState.Query = ":Name"
            InternalState.QueryState.Cursor = 5
            PropertySearch = PropertySearch.Rotate("Na", Seq.cycle [ "Name"; "Names" ]) }

    let struct (state, context) = state |> Query.prepare

    let wordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"

    let properties = [ "Key"; "Value"; "Length"; "Name"; "Type"; "Names" ]

    [<Benchmark(Baseline = true)>]
    member __.invokeAction_Noop() =
        Action.Noop |> Handle.invokeAction wordDelimiters [] state context

    [<Benchmark>]
    member __.invokeAction_AddQuery() =
        Action.AddQuery "a" |> Handle.invokeAction wordDelimiters [] state context

    [<Benchmark>]
    member __.invokeAction_BackwardChar() =
        Action.BackwardChar |> Handle.invokeAction wordDelimiters [] state context

    [<Benchmark>]
    member __.invokeAction_BackwardWord() =
        Action.BackwardWord
        |> Handle.invokeAction wordDelimiters [] stateForWord context

    [<Benchmark>]
    member __.invokeAction_DeleteBackwardChar() =
        Action.DeleteBackwardChar |> Handle.invokeAction wordDelimiters [] state context

    [<Benchmark>]
    member __.invokeAction_DeleteBackwardWord() =
        Action.DeleteBackwardWord
        |> Handle.invokeAction wordDelimiters [] stateForWord context

    [<Benchmark>]
    member __.invokeAction_SelectBackwardChar() =
        Action.SelectBackwardChar |> Handle.invokeAction wordDelimiters [] state context

    [<Benchmark>]
    member __.invokeAction_SelectBackwardWord() =
        Action.SelectBackwardWord
        |> Handle.invokeAction wordDelimiters [] stateForWord context

    [<Benchmark>]
    member __.invokeAction_RotateMatcher() =
        Action.RotateMatcher |> Handle.invokeAction wordDelimiters [] state context

    [<Benchmark>]
    member __.invokeAction_CompleteProperty_NoSearch() =
        Action.CompleteProperty |> Handle.invokeAction wordDelimiters [] state context

    [<Benchmark>]
    member __.invokeAction_CompleteProperty_Search() =
        Action.CompleteProperty
        |> Handle.invokeAction wordDelimiters properties stateForPropertySearch context

    [<Benchmark>]
    member __.invokeAction_CompleteProperty_Rotate() =
        Action.CompleteProperty
        |> Handle.invokeAction wordDelimiters properties stateForPropertyRotate context

[<MemoryDiagnoser>]
type QueryRunBenchmarks() =
    let props = Map [ ("length", "Length") ]

    let state =
        { QueryState =
            { Query = ""
              Cursor = 0
              WindowBeginningCursor = 0
              WindowWidth = 0
              InputMode = InputMode.Input }
          QueryCondition =
            { Matcher = Matcher.Match
              Operator = Operator.And
              CaseSensitive = false
              Invert = false }
          PropertySearch = PropertySearch.NoSearch
          SuppressProperties = false
          Refresh = Refresh.NotRequired
          QueryCache = ValueNone }

    [<Params(100, 1000)>]
    member val EntryCount = 0 with get, set

    [<Params(1, 5, 10)>]
    member val QueryCount = 0 with get, set

    member val NormalContext: QueryContext =
        { Queries = []
          Operator = Operator.And } with get, set

    member val PropertyContext: QueryContext =
        { Queries = []
          Operator = Operator.And } with get, set

    member val Objects: Entry pseq = PSeq.empty with get, set
    member val Dicts: Entry pseq = PSeq.empty with get, set

    [<GlobalSetup>]
    member __.GlobalSetup() =
        __.NormalContext <-
            { state with
                InternalState.QueryState.Query = seq { 0 .. __.QueryCount } |> Seq.map string |> String.concat " " }
            |> Query.prepare
            |> snd'

        __.PropertyContext <-
            { state with
                InternalState.QueryState.Query =
                    seq { 0 .. __.QueryCount }
                    |> Seq.map (fun x -> $":Length {x}")
                    |> String.concat " " }
            |> Query.prepare
            |> snd'

        __.Objects <-
            seq { 1 .. __.EntryCount }
            |> Seq.map (string >> PSObject.AsPSObject >> Entry.Obj)
            |> PSeq.ofSeq

        __.Dicts <-
            seq { 1 .. __.EntryCount }
            |> Seq.map (fun x -> ("key", x) |> DictionaryEntry |> Entry.Dict)
            |> PSeq.ofSeq

    [<Benchmark>]
    member __.run_obj_normal() =
        Query.run __.NormalContext __.Objects props |> PSeq.length |> ignore

    [<Benchmark>]
    member __.run_dict_normal() =
        Query.run __.NormalContext __.Dicts props |> PSeq.length |> ignore

    [<Benchmark>]
    member __.run_obj_property() =
        Query.run __.PropertyContext __.Objects props |> PSeq.length |> ignore

    [<Benchmark>]
    member __.run_dict_property() =
        Query.run __.PropertyContext __.Dicts props |> PSeq.length |> ignore

[<MemoryDiagnoser>]
type QueryPrepareBenchmarks() =
    let state =
        { QueryState =
            { Query = ""
              Cursor = 0
              WindowBeginningCursor = 0
              WindowWidth = 0
              InputMode = InputMode.Input }
          QueryCondition =
            { Matcher = Matcher.Match
              Operator = Operator.And
              CaseSensitive = false
              Invert = false }
          PropertySearch = PropertySearch.NoSearch
          SuppressProperties = false
          Refresh = Refresh.NotRequired
          QueryCache = ValueNone }

    [<Benchmark>]
    member __.prepare() = Query.prepare state

[<MemoryDiagnoser>]
type QueryBenchmarks() =
    let state =
        { QueryState =
            { Query = ""
              Cursor = 0
              WindowBeginningCursor = 0
              WindowWidth = 0
              InputMode = InputMode.Input }
          QueryCondition =
            { Matcher = Matcher.Match
              Operator = Operator.And
              CaseSensitive = false
              Invert = false }
          PropertySearch = PropertySearch.NoSearch
          SuppressProperties = false
          Refresh = Refresh.NotRequired
          QueryCache = ValueNone }

    let context = Query.prepare state |> snd'

    let sampleProperties =
        [ "Length"; "Name"; "Type"; "Value"; "Key"; "Path"; "Count"; "Content" ]

    [<Params(1, 3, 5, 7)>]
    member val QueryCount = 0 with get, set

    member val NormalState = state with get, set
    member val NormalContext = context with get, set
    member val PropertyState = state with get, set
    member val PropertyContext = context with get, set

    [<GlobalSetup>]
    member __.GlobalSetup() =
        __.NormalState <-
            { state with
                InternalState.QueryState.Query = seq { 0 .. __.QueryCount } |> Seq.map string |> String.concat " " }

        __.NormalContext <- __.NormalState |> Query.prepare |> snd'

        __.PropertyState <-
            { state with
                InternalState.QueryState.Query =
                    seq { 0 .. __.QueryCount }
                    |> Seq.mapi (fun x -> fun _ -> $":{sampleProperties[x % sampleProperties.Length]} {x}")
                    |> String.concat " " }

        __.PropertyContext <- __.PropertyState |> Query.prepare |> snd'

    [<Benchmark(Baseline = true)>]
    member __.prepareNormalQuery() =
        Query.QueryContext.prepareQuery __.NormalState __.NormalContext |> ignore

    [<Benchmark>]
    member __.preparePropertyQuery() =
        Query.QueryContext.prepareQuery __.PropertyState __.PropertyContext |> ignore

[<MemoryDiagnoser>]
type PocofInteractBenchmarks() =
    let prompt = ">"

    let state: InternalState =
        { QueryState =
            { Query = "foo"
              Cursor = 3
              WindowBeginningCursor = 0
              WindowWidth = 0
              InputMode = InputMode.Input }
          QueryCondition =
            { Matcher = Matcher.Match
              Operator = Operator.And
              CaseSensitive = true
              Invert = false }
          PropertySearch = PropertySearch.NoSearch
          SuppressProperties = false
          Refresh = Refresh.Required
          QueryCache = ValueNone }

    let publishEvent _ = ()

    [<Params(10, 100, 1000)>]
    member val EntryCount = 0 with get, set

    [<Params(1, 3, 5, 7, 9)>]
    member val QueryCount = 0 with get, set

    member val Objects: Entry pseq = PSeq.empty with get, set
    member val Dicts: Entry pseq = PSeq.empty with get, set
    member val Keys: ConsoleKeyInfo option list = [] with get, set

    [<GlobalSetup>]
    member __.GlobalSetup() =
        __.Objects <-
            seq { 1 .. __.EntryCount }
            |> Seq.map (string >> PSObject.AsPSObject >> Entry.Obj)
            |> PSeq.ofSeq

        __.Dicts <-
            seq { 1 .. __.EntryCount }
            |> Seq.map (fun x -> ("key", x) |> DictionaryEntry |> Entry.Dict)
            |> PSeq.ofSeq

        __.Keys <-
            [ __.QueryCount .. 1 ]
            |> List.collect (fun x ->
                [ match x with
                  | 1
                  | 3
                  | 5
                  | 7
                  | 9 ->
                      let c = x |> (+) 48 |> char
                      MockRawUI.ConsoleKey c (Enum.Parse(typeof<ConsoleKey>, c.ToString()) :?> ConsoleKey)
                  | _ -> None
                  MockRawUI.ConsoleKey ' ' ConsoleKey.Spacebar ])
            |> List.append [ MockRawUI.ConsoleKey '\000' ConsoleKey.Enter ]
            |> List.rev

    [<Benchmark>]
    member __.interact_obj() =
        let rui = new MockRawUI(60, 30, __.Keys)

        let config: InternalConfig =
            { NotInteractive = true
              Layout = Layout.TopDown
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        use buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        // NOTE: use Seq.length to force strict evaluation of the sequence
        Pocof.interact config state buff publishEvent __.Objects |> Seq.length |> ignore

    [<Benchmark>]
    member __.interact_dict() =
        let rui = new MockRawUI(60, 30, __.Keys)

        let config: InternalConfig =
            { NotInteractive = true
              Layout = Layout.TopDown
              Keymaps = Keys.defaultKeymap
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              Prompt = prompt
              PromptLength = prompt |> String.length
              Properties = []
              PropertiesMap = Map [] }

        use buff = Screen.init (fun _ -> rui) (fun _ -> Seq.empty) config.Layout prompt
        // NOTE: use Seq.length to force strict evaluation of the sequence
        Pocof.interact config state buff publishEvent __.Dicts |> Seq.length |> ignore

[<MemoryDiagnoser>]
type DataBenchmarks() =
    let queryState =
        { Query = ":Name foo :Value bar"
          Cursor = 13
          WindowBeginningCursor = 0
          WindowWidth = 0
          InputMode = InputMode.Input }

    let queryCondition =
        { Matcher = Matcher.Match
          Operator = Operator.And
          CaseSensitive = false
          Invert = false }

    [<Literal>]
    let queryString = ":Name foo"

    [<Benchmark>]
    member __.Action_fromString() =
        Action.fromString "CompleteProperty" |> ignore

    [<Benchmark>]
    member __.Operator_fromString() = Operator.fromString "And" |> ignore

    [<Benchmark>]
    member __.QueryState_getCurrentProperty() =
        QueryState.getCurrentProperty queryState |> ignore

    [<Benchmark>]
    member __.QueryCondition_toString() =
        QueryCondition.toString queryCondition |> ignore

    [<Benchmark>]
    member __.Prefix_colon() =
        queryString
        |> function
            | Prefix ":" x -> x |> ignore
            | _ -> ()

[<MemoryDiagnoser>]
type CollectionAddBenchmarks() =

    [<Params(100, 10000, 1000000)>]
    member val EntryCount = 0 with get, set

    member val Objects = Array.empty with get, set

    [<GlobalSetup>]
    member __.GlobalSetup() =
        __.Objects <-
            seq { 1 .. __.EntryCount }
            |> Seq.map (string >> PSObject.AsPSObject >> Entry.Obj)
            |> Array.ofSeq

    [<Benchmark(Baseline = true)>]
    member __.ConcurrentQueue() =
        let cq = new ConcurrentQueue<Entry>()
        __.Objects |> Array.iter (fun obj -> cq.Enqueue obj)

    [<Benchmark>]
    member __.SpscAppendOnlyBuffer() =
        let buffer = new SpscAppendOnlyBuffer<Entry>()
        __.Objects |> Array.iter (fun obj -> buffer.Add obj)

[<MemoryDiagnoser>]
type CollectionIterateBenchmarks() =

    [<Params(100, 10000, 1000000)>]
    member val EntryCount = 0 with get, set

    member val Queue = new ConcurrentQueue<Entry>() with get, set
    member val spsc = new SpscAppendOnlyBuffer<Entry>() with get, set

    [<GlobalSetup>]
    member __.GlobalSetup() =
        seq { 1 .. __.EntryCount }
        |> Seq.iter (fun i ->
            let obj = i |> (string >> PSObject.AsPSObject >> Entry.Obj)
            __.Queue.Enqueue obj
            __.spsc.Add obj)

    [<Benchmark(Baseline = true)>]
    member __.ConcurrentQueue_iterate() = __.Queue |> Seq.iter (fun _ -> ())

    [<Benchmark>]
    member __.SpscAppendOnlyBuffer_iterate() = __.spsc |> Seq.iter (fun _ -> ())

[<MemoryDiagnoser>]
type CollectionBenchmarks() =

    let iter predicate source =
        ParallelEnumerable.ForAll(PSeq.ofSeq source, Action<_>(predicate))

    [<Params(100, 10000, 1000000)>]
    member val EntryCount = 0 with get, set

    [<Params(1, 4, 8)>]
    member val FetchCount = 0 with get, set

    member val Objects = Array.empty with get, set

    [<GlobalSetup>]
    member __.GlobalSetup() =
        __.Objects <-
            seq { 1 .. __.EntryCount }
            |> Seq.map (string >> PSObject.AsPSObject >> Entry.Obj)
            |> Array.ofSeq

    [<Benchmark(Baseline = true)>]
    member __.ConcurrentQueue() =
        let cq = new ConcurrentQueue<Entry>()
        __.Objects |> Array.iter (fun obj -> cq.Enqueue obj)

        for _ in 1 .. __.FetchCount do
            cq |> iter (fun _ -> ())

    [<Benchmark>]
    member __.SpscAppendOnlyBuffer() =
        let buffer = new SpscAppendOnlyBuffer<Entry>()
        __.Objects |> Array.iter (fun obj -> buffer.Add obj)

        for _ in 1 .. __.FetchCount do
            buffer |> iter (fun _ -> ())
