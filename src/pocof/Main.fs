namespace pocof

open System

open PocofData
open PocofHandle

open System.Management.Automation
open System.Collections

module Pocof =
    type Entry = PocofData.Entry
    type KeyPattern = PocofData.KeyPattern
    type Action = PocofData.Action
    type Matcher = PocofData.Matcher
    type Operator = PocofData.Operator
    type Layout = PocofData.Layout

    type RawUI = PocofScreen.RawUI

    let convertKeymaps = PocofAction.convertKeymaps
    let initConfig = PocofData.initConfig

    type LoopFixedArguments =
        { keymaps: Map<KeyPattern, Action>
          input: Entry list
          propMap: Map<string, string>
          writeScreen: PocofScreen.WriteScreen
          getKey: unit -> ConsoleKeyInfo list
          getConsoleWidth: unit -> int
          getLengthInBufferCells: string -> int }

    [<TailCall>]
    let rec private searchBeginningCursorRecursive (getLengthInBufferCells: string -> int) (state: QueryState) =
        let l =
            getLengthInBufferCells state.Query.[state.WindowBeginningCursor .. state.Cursor - 1]

        match l with
        | bx when bx <= state.WindowWidth ->
#if DEBUG
            Logger.logFile [ $"bx '{bx}' WindowWidth '{state.WindowWidth}' Cursor '{state.Cursor}' WindowBeginningX '{state.WindowBeginningCursor}'" ]
#endif
            state.WindowBeginningCursor
        | _ ->
            searchBeginningCursorRecursive
                getLengthInBufferCells
                { state with WindowBeginningCursor = state.WindowBeginningCursor + 1 }

    let calculateWindowBeginningCursor (getLengthInBufferCells: string -> int) (state: QueryState) =
#if DEBUG
        Logger.logFile [ $"Cursor '{state.Cursor}' WindowBeginningX '{state.WindowBeginningCursor}' WindowWidth '{state.WindowWidth}'" ]
#endif
        match state.WindowBeginningCursor > state.Cursor with
        | true -> state.Cursor
        | _ ->
            let wx =
                let l =
                    getLengthInBufferCells state.Query.[state.WindowBeginningCursor .. state.Cursor]

                match l > state.WindowWidth with
                | true -> searchBeginningCursorRecursive getLengthInBufferCells state
                | _ -> state.WindowBeginningCursor

#if DEBUG
            Logger.logFile [ $"wx '{wx}' Cursor '{state.Cursor}' WindowBeginningX '{state.WindowBeginningCursor}' WindowWidth '{state.WindowWidth}'" ]
#endif
            wx

    let adjustQueryWindow (args: LoopFixedArguments) (state: InternalState) =
        { state with
            InternalState.QueryState.WindowBeginningCursor =
                calculateWindowBeginningCursor args.getLengthInBufferCells state.QueryState }

    let queryAndRender
        (args: LoopFixedArguments)
        (results: Entry list)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =

        match state.Refresh with
        | NotRequired -> results, state
        | _ ->
            let results = PocofQuery.run context args.input args.propMap

            let state =
                state
                |> InternalState.updateFilteredCount (List.length results)
                |> adjustQueryWindow args

            args.writeScreen state results
            <| match state.SuppressProperties with
               | true -> Ok []
               | _ -> PocofQuery.props state

            results, state

    [<TailCall>]
    let rec loop
        (args: LoopFixedArguments)
        (results: Entry list)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =

        let results, state = queryAndRender args results state pos context

        args.getKey ()
        |> PocofAction.get args.keymaps
        |> function
            | Cancel -> []
            | Finish -> unwrap results
            | action ->
                action
                |> invokeAction
                    (state
                     |> InternalState.updateConsoleWidth (args.getConsoleWidth ()))
                    pos
                    context
                |||> loop args results

    let interact
        (conf: InternalConfig)
        (state: InternalState)
        (pos: Position)
        (rui: unit -> PocofScreen.IRawUI)
        (invoke: obj list -> string seq)
        (input: Entry list)
        =

        let state, context = PocofQuery.prepare state

        let propMap =
            state.Properties
            |> List.map (fun p -> String.lower p, p)
            |> Map.ofList

        match conf.NotInteractive with
        | true ->
            let l = PocofQuery.run context input propMap
            unwrap l
        | _ ->
            use buff = PocofScreen.init rui invoke

            let args =
                { keymaps = conf.Keymaps
                  input = input
                  propMap = propMap
                  writeScreen =
                    match conf.Layout with
                    | TopDown -> buff.writeTopDown
                    | BottomUp -> buff.writeBottomUp
                  getKey = buff.getKey
                  getConsoleWidth = buff.getConsoleWidth
                  getLengthInBufferCells = buff.GetLengthInBufferCells }

            loop args input state pos context

    let buildInput acc (input: PSObject array) =
        input
        |> Seq.fold
            (fun acc o ->
                match o.BaseObject with
                | :? IDictionary as dct ->
                    Seq.cast<DictionaryEntry> dct
                    |> Seq.fold (fun a d -> Dict d :: a) acc
                | _ -> Obj(PSObject o) :: acc)
            acc

    let buildProperties acc (input: PSObject array) =
        Set.union acc
        <| (input
        |> Seq.collect (fun o ->
            match o.BaseObject with
            | :? IDictionary as dct ->
                match Seq.cast<DictionaryEntry> dct with
                | s when Seq.isEmpty s -> Seq.empty
                | s ->
                    s
                    |> Seq.head
                    |> PSObject.AsPSObject
                    |> _.Properties
            | _ -> o.Properties)
        |> Seq.map _.Name
        |> Set.ofSeq)
