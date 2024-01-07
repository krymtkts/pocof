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
          getConsoleWidth: unit -> int }

    let calculateWindowBeginningCursor (state: QueryState) =
        let wx =
            match state.Cursor - state.WindowBeginningCursor with
            | bx when bx < 0 -> state.WindowBeginningCursor + bx
            | bx when bx > state.WindowWidth -> state.Cursor - state.WindowWidth
            | _ -> state.WindowBeginningCursor

#if DEBUG
        Logger.logFile [ $"wx '{wx}' Cursor '{state.Cursor}' WindowBeginningCursor '{state.WindowBeginningCursor}' WindowWidth '{state.WindowWidth}'" ]
#endif
        wx

    let adjustQueryWindow (state: InternalState) =
        { state with InternalState.QueryState.WindowBeginningCursor = calculateWindowBeginningCursor state.QueryState }

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
                |> adjustQueryWindow
                |> InternalState.updateFilteredCount (List.length results)

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
                  getConsoleWidth = buff.getConsoleWidth }

            loop args input state pos context
