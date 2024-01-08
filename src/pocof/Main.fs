namespace pocof

open System
open System.Management.Automation.Host

open PocofData
open PocofHandle

module Pocof =
    type LoopFixedArguments =
        { keymaps: Map<KeyPattern, Action>
          input: Entry list
          propMap: Map<string, string>
          writeScreen: PocofScreen.WriteScreen
          getKey: unit -> ConsoleKeyInfo list
          getConsoleWidth: unit -> int
          getLengthInBufferCells: string -> int }

    [<TailCall>]
    let rec searchBeginningCursorRecursive (getLengthInBufferCells: string -> int) (state: QueryState) =
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

                match l with
                | bx when bx < 0 -> state.WindowBeginningCursor + bx
                | bx when bx > state.WindowWidth -> searchBeginningCursorRecursive getLengthInBufferCells state
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
        (rui: PSHostRawUserInterface)
        (invoke: obj list -> seq<string>)
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
