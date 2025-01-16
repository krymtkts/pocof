namespace Pocof

open System
open System.Collections
open System.Diagnostics
open System.Management.Automation
open System.Reflection
open System.Threading

open Data
open Handle

[<RequireQualifiedAccess>]
module Pocof =
    type Entry = Data.Entry
    type KeyPattern = Data.KeyPattern
    type Action = Data.Action
    type Matcher = Data.Matcher
    type Operator = Data.Operator
    type Layout = Data.Layout
    type InternalConfig = Data.InternalConfig

    type Buff = Screen.Buff
    type IConsoleInterface = Screen.IConsoleInterface

    let convertKeymaps = Keys.convertKeymaps
    let initConfig = Data.initConfig

    [<Literal>]
    let defaultWordDelimiters = Handle.wordDelimiters

    let initRawUI psRawUI console : unit -> Screen.IRawUI =
        fun () -> new Screen.RawUI(psRawUI, console)

    let initConsoleInterface () : Screen.IConsoleInterface = new Screen.ConsoleInterface()

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<NoEquality>]
    [<Struct>]
    type RenderEvent =
        | Render of (InternalState * Entry pseq Lazy * Result<string list, string> Lazy)
        | Quit

    [<NoComparison>]
    [<NoEquality>]
    type LoopFixedArguments =
        { Keymaps: Map<KeyPattern, Action>
          Input: Entry pseq
          PublishEvent: RenderEvent -> unit
          GetKey: unit -> ConsoleKeyInfo list
          GetConsoleWidth: unit -> int
          GetLengthInBufferCells: string -> int
          WordDelimiters: string }

    [<TailCall>]
    let rec private searchBeginningCursorRecursive (getLengthInBufferCells: string -> int) (state: QueryState) =
        let l =
            getLengthInBufferCells
            <| state.Query.Substring(state.WindowBeginningCursor, state.Cursor - state.WindowBeginningCursor)

        match l with
        | bx when bx <= state.WindowWidth ->
#if DEBUG
            Logger.LogFile
                [ $"LengthInBufferCells '{bx}' WindowWidth '{state.WindowWidth}' Cursor '{state.Cursor}' WindowBeginningCursor '{state.WindowBeginningCursor}'" ]
#endif
            state.WindowBeginningCursor
        | _ ->
            searchBeginningCursorRecursive
                getLengthInBufferCells
                { state with
                    WindowBeginningCursor = state.WindowBeginningCursor + 1 }

    let calculateWindowBeginningCursor (getLengthInBufferCells: string -> int) (state: QueryState) =
#if DEBUG
        Logger.LogFile
            [ $"Cursor '{state.Cursor}' WindowBeginningCursor '{state.WindowBeginningCursor}' WindowWidth '{state.WindowWidth}'" ]
#endif
        match state.WindowBeginningCursor > state.Cursor with
        | true -> state.Cursor
        | _ ->
            let wx =
                let l =
                    getLengthInBufferCells
                    <| state.Query.Substring(state.WindowBeginningCursor, state.Cursor - state.WindowBeginningCursor)

                match l > state.WindowWidth with
                | true -> searchBeginningCursorRecursive getLengthInBufferCells state
                | _ -> state.WindowBeginningCursor

#if DEBUG
            Logger.LogFile
                [ $"WindowBeginningCursor '{wx}' Cursor '{state.Cursor}' WindowBeginningCursor '{state.WindowBeginningCursor}' WindowWidth '{state.WindowWidth}'" ]
#endif
            wx

    let private adjustQueryWindow (getLengthInBufferCells: string -> int) (state: InternalState) =
        { state with
            InternalState.QueryState.WindowBeginningCursor =
                calculateWindowBeginningCursor getLengthInBufferCells state.QueryState }

    let query
        (args: LoopFixedArguments)
        (results: Entry pseq Lazy)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =

        match state.Refresh with
        | Refresh.NotRequired -> results, state
        | _ ->
            let results = lazy Query.run context args.Input state.PropertyMap
            let state = state |> adjustQueryWindow args.GetLengthInBufferCells
            let props = lazy Query.props state
            (state, results, props) |> RenderEvent.Render |> args.PublishEvent
            results, state

    [<TailCall>]
    let rec private loop
        (args: LoopFixedArguments)
        (results: Entry pseq Lazy)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =

        let results, state = query args results state pos context

        args.GetKey()
        |> Keys.get args.Keymaps
        |> function
            | Action.Cancel ->
                args.PublishEvent RenderEvent.Quit
                seq []
            | Action.Finish ->
                args.PublishEvent RenderEvent.Quit
                unwrap results.Value
            | action ->
                // NOTE: update the console width before invokeAction because users can modify the console width during blocking by args.GetKey.
                action
                |> invokeAction args.WordDelimiters (state |> InternalState.updateConsoleWidth (args.GetConsoleWidth())) pos context
                |||> loop args results

    let interact
        (conf: InternalConfig)
        (state: InternalState)
        (pos: Position)
        (buff: Screen.Buff)
        // (buff: Screen.Buff option)
        (publish: RenderEvent -> unit)
        (input: Entry seq)
        =

        let state, context = Query.prepare state
        let input = input |> PSeq.ofSeq

        let args =
            { Keymaps = conf.Keymaps
              Input = input
              PublishEvent = publish
              GetKey = buff.GetKey
              GetConsoleWidth = buff.GetConsoleWidth
              GetLengthInBufferCells = buff.GetLengthInBufferCells
              WordDelimiters = conf.WordDelimiters }

        loop args (lazy input) state pos context

    let interactOnce (state: InternalState) (input: Entry seq) =

        let state, context = Query.prepare state
        let input = input |> PSeq.ofSeq

        Query.run context input state.PropertyMap |> unwrap

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<NoEquality>]
    [<Struct>]
    type RenderMessage =
        | None
        | Received of RenderEvent

    [<Sealed>]
    type RenderHandler() =
        let renderStack: RenderEvent Concurrent.ConcurrentStack =
            Concurrent.ConcurrentStack()

        [<TailCall>]
        let rec getLatestEvent (h: RenderEvent) (es: RenderEvent list) =
            match h with
            | RenderEvent.Quit -> h
            | h ->
                match es with
                | [] -> h
                | e :: es ->
                    match e with
                    | RenderEvent.Quit -> e
                    | _ -> getLatestEvent h es

        let getLatestEvent (es: RenderEvent list) =
            match es with
            | [] -> RenderMessage.None
            | h :: es -> getLatestEvent h es |> RenderMessage.Received

        member __.Publish = renderStack.Push

        member __.Receive() =
            let items =
                match renderStack.Count with
                // NOTE: case of 0 is required for .NET Framework forward compatibility.
                // NOTE: .NET does not raise an error, but it does not match the documentation.
                | 0 -> []
                | c ->
                    let items = Array.zeroCreate<RenderEvent> c
                    renderStack.TryPopRange(items) |> ignore
                    items |> Array.toList

            items |> getLatestEvent

    [<TailCall>]
    let rec render (buff: Screen.Buff) (handler: RenderHandler) (conf: InternalConfig) =
        match handler.Receive() with
        | RenderMessage.None ->
            Thread.Sleep 10
            render buff handler conf
        | RenderMessage.Received RenderEvent.Quit -> ()
        | RenderMessage.Received(RenderEvent.Render(state, entries, props)) ->
            buff.WriteScreen conf.Layout state entries.Value props.Value
            render buff handler conf

    let stopUpstreamCommandsException (exp: Type) (cmdlet: Cmdlet) =
        let stopUpstreamCommandsException =
            Activator.CreateInstance(
                exp,
                BindingFlags.Default
                ||| BindingFlags.CreateInstance
                ||| BindingFlags.Instance
                ||| BindingFlags.Public,
                null,
                [| cmdlet :> obj |],
                null
            )
            :?> Exception

        stopUpstreamCommandsException

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<NoEquality>]
    [<Struct>]
    type RenderProcess =
        | Noop
        | Rendered of (InternalState * Entry pseq Lazy * Result<string list, string> Lazy)
        | StopUpstreamCommands

    let renderOnce (conf: InternalConfig) (handler: RenderHandler) (buff: Screen.Buff) =
        match handler.Receive() with
        | RenderMessage.None -> RenderProcess.Noop
        | RenderMessage.Received RenderEvent.Quit -> RenderProcess.StopUpstreamCommands
        | RenderMessage.Received(RenderEvent.Render(state, entries, props)) ->
            buff.WriteScreen conf.Layout state entries.Value props.Value
            RenderProcess.Rendered(state, entries, props)

    [<Sealed>]
    type Periodic(conf, handler, buff, cancelAction) =
        let conf: InternalConfig = conf
        let handler: RenderHandler = handler
        let buff: Screen.Buff = buff
        let cancelAction: unit -> unit = cancelAction
        let stopwatch = Stopwatch()
        let idlingStopwatch = Stopwatch()

        let mutable latest: (InternalState * Entry pseq Lazy * Result<string list, string> Lazy) option =
            None

        let renderAgain (state: InternalState, result: Entry pseq Lazy, props: Result<string list, string> Lazy) =
            let state =
                state
                // NOTE: adjust the console width before writing the screen.
                |> InternalState.updateConsoleWidth (buff.GetConsoleWidth())
                |> adjustQueryWindow buff.GetLengthInBufferCells

            buff.WriteScreen conf.Layout state result.Value props.Value

        let (|Cancelled|_|) =
            function
            | RenderProcess.Noop ->
                if idlingStopwatch.ElapsedMilliseconds >= 1000 then
                    idlingStopwatch.Reset()
                    latest |> Option.iter renderAgain
                    idlingStopwatch.Start()
                    None
                else
                    None
            | RenderProcess.Rendered e ->
                latest <- Some e
                stopwatch.Restart()
                idlingStopwatch.Restart()
                None
            | RenderProcess.StopUpstreamCommands ->
                stopwatch.Stop()
                idlingStopwatch.Stop()
                Some()

        do
            stopwatch.Start()
            idlingStopwatch.Start()

        member __.Stop() =
            stopwatch.Stop()
            idlingStopwatch.Stop()
            latest |> Option.iter renderAgain

        member __.Render() =
            if stopwatch.ElapsedMilliseconds >= 10 then
                renderOnce conf handler buff
                |> function
                    | Cancelled _ -> cancelAction ()
                    | _ -> ()

    [<Interface>]
    type IInputStore =
        abstract member Add: PSObject -> unit
        abstract member GetEntries: unit -> Entry seq
        abstract member Count: unit -> int

    [<AbstractClass>]
    type AbstractInputStore() =
        member val Store: Entry Concurrent.ConcurrentQueue = Concurrent.ConcurrentQueue()

        abstract member AddEntry: Entry -> unit

        interface IInputStore with
            member __.Add input =
                match input.BaseObject with
                | :? IDictionary as dct ->
                    for d in Seq.cast<DictionaryEntry> dct do
                        d |> Entry.Dict |> __.AddEntry
                | _ -> input |> Entry.Obj |> __.AddEntry

            member __.GetEntries() = __.Store
            member __.Count() = __.Store.Count

    [<Sealed>]
    type NormalInputStore() =
        inherit AbstractInputStore()

        override __.AddEntry entry = entry |> base.Store.Enqueue

    [<Sealed>]
    type UniqueInputStore() =
        inherit AbstractInputStore()

        let keys: Concurrent.ConcurrentDictionary<Entry, unit> =
            Concurrent.ConcurrentDictionary()

        override __.AddEntry entry =
            if (entry, ()) |> keys.TryAdd then
                entry |> __.Store.Enqueue

    let getInputStore (unique: bool) =
        match unique with
        | true -> UniqueInputStore() :> IInputStore
        | _ -> NormalInputStore() :> IInputStore

    let buildProperties (exists: string -> bool) (add: string * string seq -> Unit) (input: PSObject) =
        let name = input.BaseObject.GetType().FullName

        if name |> exists |> not then
            let props =
                match input.BaseObject with
                | :? IDictionary as dct ->
                    match Seq.cast<DictionaryEntry> dct with
                    | s when Seq.isEmpty s -> Seq.empty
                    | s -> s |> Seq.head |> _.GetType().GetProperties() |> Seq.map _.Name
                | _ -> input.Properties |> Seq.map _.Name

            (name, props) |> add

    [<Sealed>]
    type PropertyStore() =
        let typeNameDictionary: Concurrent.ConcurrentDictionary<string, unit> =
            Concurrent.ConcurrentDictionary()

        let propertiesDictionary: Concurrent.ConcurrentDictionary<string, unit> =
            Concurrent.ConcurrentDictionary()

        let properties: string Concurrent.ConcurrentQueue = Concurrent.ConcurrentQueue()

        let propertiesMap: Concurrent.ConcurrentDictionary<string, string> =
            Concurrent.ConcurrentDictionary(StringComparer.OrdinalIgnoreCase)

        member __.ContainsKey = propertiesDictionary.ContainsKey

        member __.Add(name, props) =
            if typeNameDictionary.TryAdd(name, ()) then
                for prop in props do
                    if propertiesDictionary.TryAdd(prop, ()) then
                        prop |> properties.Enqueue
                        propertiesMap.TryAdd(prop, prop) |> ignore

        member __.GetProperties() = properties
        member __.GetPropertyMap() = propertiesMap

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<NoEquality>]
    [<Struct>]
    type Termination =
        | Normal
        | Force

    let init
        (psRawUI: Host.PSHostRawUserInterface)
        (console: Screen.IConsoleInterface)
        invoke
        cancelAction
        (handler: RenderHandler)
        (entries: unit -> seq<Entry>)
        (p: IncomingParameters)
        =
        let conf, state, pos = initConfig p

        match conf.NotInteractive with
        | true ->
            let render () = ()
            let waitResult (_) = interactOnce state (entries ())
            render, waitResult
        | _ ->
            let buff = Screen.init (initRawUI psRawUI console) invoke conf.Layout

            let mainTask =
                async { return interact conf state pos buff handler.Publish <| entries () }
                |> Async.StartAsTask

            let periodic = Periodic(conf, handler, buff, cancelAction)
            let renderPeriodic () = periodic.Render()

            let waitResult (term: Termination) =
                periodic.Stop()

                match term with
                | Termination.Force -> ()
                | _ -> render buff handler conf

                buff :> IDisposable |> _.Dispose()
                mainTask.Result

            renderPeriodic, waitResult
