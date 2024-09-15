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

    let initRawUI psRawUI console : unit -> Screen.IRawUI =
        fun (_: unit) -> new Screen.RawUI(psRawUI, console)

    let initConsoleInterface () : Screen.IConsoleInterface = new Screen.ConsoleInterface()

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<NoEquality>]
    [<Struct>]
    type RenderEvent =
        | Render of (InternalState * Entry pseq * Result<string list, string>)
        | Quit

    [<NoComparison>]
    [<NoEquality>]
    type LoopFixedArguments =
        { Keymaps: Map<KeyPattern, Action>
          Input: Entry pseq
          PublishEvent: RenderEvent -> unit
          GetKey: unit -> ConsoleKeyInfo list
          GetConsoleWidth: unit -> int
          GetLengthInBufferCells: string -> int }

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

    let adjustQueryWindow (getLengthInBufferCells: string -> int) (state: InternalState) =
        { state with
            InternalState.QueryState.WindowBeginningCursor =
                calculateWindowBeginningCursor getLengthInBufferCells state.QueryState }

    let query
        (args: LoopFixedArguments)
        (results: Entry seq)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =

        match state.Refresh with
        | Refresh.NotRequired -> results, state
        | _ ->
            let results = Query.run context args.Input state.PropertyMap

            let state =
                state
                |> InternalState.updateFilteredCount (PSeq.length results)
                |> adjustQueryWindow args.GetLengthInBufferCells

            (state, results, Query.props state) |> RenderEvent.Render |> args.PublishEvent

            results, state

    [<TailCall>]
    let rec loop
        (args: LoopFixedArguments)
        (results: Entry seq)
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
                unwrap results
            | action ->
                // NOTE: update the console width before invokeAction because users can modify the console width during blocking by args.GetKey.
                action
                |> invokeAction (state |> InternalState.updateConsoleWidth (args.GetConsoleWidth())) pos context
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

        // match buff with
        // | None -> Query.run context input state.PropertyMap |> unwrap
        // | Some buff ->
        let args =
            { Keymaps = conf.Keymaps
              Input = input
              PublishEvent = publish
              GetKey = buff.GetKey
              GetConsoleWidth = buff.GetConsoleWidth
              GetLengthInBufferCells = buff.GetLengthInBufferCells }

        loop args input state pos context

    let interactOnce
        // (conf: InternalConfig)
        (state: InternalState)
        // (pos: Position)
        // (buff: Screen.Buff)
        // (buff: Screen.Buff option)
        // (publish: RenderEvent -> unit)
        (input: Entry seq)
        =

        let state, context = Query.prepare state
        let input = input |> PSeq.ofSeq

        // match buff with
        // | None ->
        Query.run context input state.PropertyMap |> unwrap
    // | Some buff ->
    // let args =
    //     { Keymaps = conf.Keymaps
    //       Input = input
    //       PublishEvent = publish
    //       GetKey = buff.GetKey
    //       GetConsoleWidth = buff.GetConsoleWidth
    //       GetLengthInBufferCells = buff.GetLengthInBufferCells }

    // loop args input state pos context
    let initScreen (rui: unit -> Screen.IRawUI) (invoke: obj seq -> string seq) (conf: InternalConfig) =
        match conf.NotInteractive with
        | true -> None
        | _ -> Screen.init rui invoke conf.Layout |> Some

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

        member __.Publish = renderStack.Push

        member __.Receive() =
            // NOTE: Currently TryPop and Clear combination isn't atomic. Even if you receive after popping, it is ignored.
            match renderStack.TryPop() with
            | false, _ -> RenderMessage.None
            | _, e ->
                renderStack.Clear()
                RenderMessage.Received e

    [<TailCall>]
    let rec render (buff: Screen.Buff) (handler: RenderHandler) (conf: InternalConfig) =
        // buff
        // |> function
        //     | None -> ()
        //     | Some b ->
        match handler.Receive() with
        | RenderMessage.None ->
            Thread.Sleep 10
            render buff handler conf
        | RenderMessage.Received RenderEvent.Quit -> ()
        | RenderMessage.Received(RenderEvent.Render e) ->
            e |||> buff.WriteScreen conf.Layout
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
        | Rendered of (InternalState * Entry pseq * Result<string list, string>)
        | StopUpstreamCommands

    let renderOnce (conf: InternalConfig) (handler: RenderHandler) (buff: Screen.Buff) =
        match handler.Receive() with
        | RenderMessage.None -> RenderProcess.Noop
        | RenderMessage.Received RenderEvent.Quit -> RenderProcess.StopUpstreamCommands
        | RenderMessage.Received(RenderEvent.Render e) ->
            e |||> buff.WriteScreen conf.Layout
            RenderProcess.Rendered e

    [<Sealed>]
    type Periodic(conf, handler, buff, cancelAction) =
        let conf: InternalConfig = conf
        let handler: RenderHandler = handler
        let buff: Screen.Buff = buff
        let cancelAction: unit -> unit = cancelAction
        let stopwatch = Stopwatch()
        let idlingStopwatch = Stopwatch()
        let mutable latest = None

        let renderAgain (state, result, props) =
            let state =
                state
                // NOTE: adjust the console width before writing the screen.
                |> InternalState.updateConsoleWidth (buff.GetConsoleWidth())
                |> InternalState.updateFilteredCount (PSeq.length result)
                |> adjustQueryWindow buff.GetLengthInBufferCells

            buff.WriteScreen conf.Layout state result props

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
                    | s -> s |> Seq.head |> PSObject.AsPSObject |> _.Properties
                | _ -> input.Properties
                |> Seq.map _.Name

            (name, props) |> add

    [<Sealed>]
    type PropertyStore() =
        let typeNameDictionary: Concurrent.ConcurrentDictionary<string, unit> =
            Concurrent.ConcurrentDictionary()

        let propertiesDictionary: Concurrent.ConcurrentDictionary<string, unit> =
            Concurrent.ConcurrentDictionary()

        let properties: string Concurrent.ConcurrentQueue = Concurrent.ConcurrentQueue()

        let propertiesMap: Concurrent.ConcurrentDictionary<string, string> =
            Concurrent.ConcurrentDictionary()

        member __.ContainsKey = propertiesDictionary.ContainsKey

        member __.Add(name, props) =
            if typeNameDictionary.TryAdd(name, ()) then
                for prop in props do
                    if propertiesDictionary.TryAdd(prop, ()) then
                        prop |> properties.Enqueue
                        propertiesMap.TryAdd(String.lower prop, prop) |> ignore

        member __.GetProperties() = properties
        member __.GetPropertyMap() = propertiesMap

    let initPocof
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
            let render () = interactOnce state (entries ())
            conf, None, render |> Some
        | _ ->
            let buff = Screen.init (initRawUI psRawUI console) invoke conf.Layout

            let mainTask =
                async { return interact conf state pos buff handler.Publish <| entries () }
                |> Async.StartAsTask

            let periodic = Periodic(conf, handler, buff, cancelAction)

            let render () =
                periodic.Stop()
                render buff handler conf
                buff :> IDisposable |> _.Dispose()
                mainTask.Result

            conf, periodic |> Some, render |> Some
