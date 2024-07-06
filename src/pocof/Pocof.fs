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
    type RenderEvent =
        | Render of (InternalState * Entry seq * Result<string list, string>)
        | Quit

    [<NoComparison>]
    [<NoEquality>]
    type LoopFixedArguments =
        { Keymaps: Map<KeyPattern, Action>
          Input: Entry seq
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
                [ $"bx '{bx}' WindowWidth '{state.WindowWidth}' Cursor '{state.Cursor}' WindowBeginningX '{state.WindowBeginningCursor}'" ]
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
            [ $"Cursor '{state.Cursor}' WindowBeginningX '{state.WindowBeginningCursor}' WindowWidth '{state.WindowWidth}'" ]
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
                [ $"wx '{wx}' Cursor '{state.Cursor}' WindowBeginningX '{state.WindowBeginningCursor}' WindowWidth '{state.WindowWidth}'" ]
#endif
            wx

    let adjustQueryWindow (args: LoopFixedArguments) (state: InternalState) =
        { state with
            InternalState.QueryState.WindowBeginningCursor =
                calculateWindowBeginningCursor args.GetLengthInBufferCells state.QueryState }

    let queryAndRender
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
                |> InternalState.updateFilteredCount (Seq.length results)
                |> adjustQueryWindow args

            (state,
             results,
             match state.SuppressProperties with
             | true -> Ok []
             | _ -> Query.props state)
            |> RenderEvent.Render
            |> args.PublishEvent

            results, state

    [<TailCall>]
    let rec loop
        (args: LoopFixedArguments)
        (results: Entry seq)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =

        let results, state = queryAndRender args results state pos context

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
                action
                |> invokeAction (state |> InternalState.updateConsoleWidth (args.GetConsoleWidth())) pos context
                |||> loop args results

    let interact
        (conf: InternalConfig)
        (state: InternalState)
        (pos: Position)
        (buff: Screen.Buff option)
        (publish: RenderEvent -> unit)
        (input: Entry seq)
        =

        let state, context = Query.prepare state

        match buff with
        | None ->
            let l = Query.run context input state.PropertyMap
            unwrap l
        | Some buff ->
            let args =
                { Keymaps = conf.Keymaps
                  Input = input
                  PublishEvent = publish
                  GetKey = buff.GetKey
                  GetConsoleWidth = buff.GetConsoleWidth
                  GetLengthInBufferCells = buff.GetLengthInBufferCells }

            loop args input state pos context

    let initScreen (rui: unit -> Screen.IRawUI) (invoke: obj seq -> string seq) (conf: InternalConfig) =
        match conf.NotInteractive with
        | true -> None
        | _ -> Screen.init rui invoke conf.Layout |> Some

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<NoEquality>]
    type RenderMessage =
        | None
        | Received of RenderEvent

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
    let rec render (conf: InternalConfig) (handler: RenderHandler) (buff: Screen.Buff option) =
        buff
        |> function
            | None -> ()
            | Some b ->
                match handler.Receive() with
                | RenderMessage.None ->
                    Thread.Sleep 10
                    render conf handler buff
                | RenderMessage.Received RenderEvent.Quit -> ()
                | RenderMessage.Received(RenderEvent.Render e) ->
#if DEBUG
                    Logger.LogFile [ $"render. length: {e |> sndOf3 |> Seq.length}" ]
#endif
                    e |||> b.WriteScreen conf.Layout
                    render conf handler buff

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
    type RenderProcess =
        | Noop
        | Rendered of (InternalState * Entry seq * Result<string list, string>)
        | StopUpstreamCommands

    let renderOnce (conf: InternalConfig) (handler: RenderHandler) (buff: Screen.Buff) =
        match handler.Receive() with
        | RenderMessage.None -> RenderProcess.Noop
        | RenderMessage.Received RenderEvent.Quit -> RenderProcess.StopUpstreamCommands
        | RenderMessage.Received(RenderEvent.Render e) ->

#if DEBUG
            Logger.LogFile [ $"renderOnce. length: {e |> sndOf3 |> Seq.length}" ]
#endif
            e |||> buff.WriteScreen conf.Layout
            RenderProcess.Rendered e

    type Periodic(conf, handler, buff) =
        let conf: InternalConfig = conf
        let handler: RenderHandler = handler
        let buff: Screen.Buff = buff
        let stopwatch = Stopwatch()
        let mutable idleRenderCount = 0
        let mutable latest = None

        let (|Cancelled|_|) =
            function
            | RenderProcess.Noop ->
                if idleRenderCount >= 10 then
                    idleRenderCount <- 0

                    latest
                    |> Option.iter (fun (state, result, props) ->
                        // TODO: encapsulate this from here.
                        let state = state |> InternalState.updateFilteredCount (Seq.length result)
                        buff.WriteScreen conf.Layout state result props)

                    None
                else
                    idleRenderCount <- idleRenderCount + 1
                    None
            | RenderProcess.Rendered e ->
                latest <- Some e
                stopwatch.Restart()
                None
            | RenderProcess.StopUpstreamCommands ->
                idleRenderCount <- 0
                stopwatch.Stop()
                Some()

        do stopwatch.Start()

        member __.Stop() =
            stopwatch.Stop()
            latest |> Option.iter (fun e -> e |||> buff.WriteScreen conf.Layout)

        member __.Render(actionForCancel: unit -> unit) =
            if stopwatch.ElapsedMilliseconds >= 10 then
                renderOnce conf handler buff
                |> function
                    | Cancelled _ -> actionForCancel ()
                    | _ -> ()

    type IInputStore =
        abstract member Add: PSObject -> unit
        abstract member GetAll: unit -> Entry seq
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

            member __.GetAll() = __.Store
            member __.Count() = __.Store.Count

    type NormalInputStore() =
        inherit AbstractInputStore()

        override __.AddEntry entry = entry |> base.Store.Enqueue

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

        member __.GetList() = properties
        member __.GetMap() = propertiesMap
