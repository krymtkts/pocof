namespace Pocof

open System
open System.Collections
open System.Management.Automation

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

    type RawUI = Screen.RawUI

    let convertKeymaps = Keys.convertKeymaps
    let initConfig = Data.initConfig

    [<NoComparison>]
    [<NoEquality>]
    type LoopFixedArguments =
        { Keymaps: Map<KeyPattern, Action>
          Input: Entry list
          PropMap: Map<string, string>
          WriteScreen: Screen.WriteScreen
          GetKey: unit -> ConsoleKeyInfo list
          GetConsoleWidth: unit -> int
          GetLengthInBufferCells: string -> int }

    [<TailCall>]
    let rec private searchBeginningCursorRecursive (getLengthInBufferCells: string -> int) (state: QueryState) =
        let l =
            getLengthInBufferCells state.Query.[state.WindowBeginningCursor .. state.Cursor - 1]

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
                    getLengthInBufferCells state.Query.[state.WindowBeginningCursor .. state.Cursor]

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
        (results: Entry list)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =

        match state.Refresh with
        | Refresh.NotRequired -> results, state
        | _ ->
            let results = Query.run context args.Input args.PropMap

            let state =
                state
                |> InternalState.updateFilteredCount (List.length results)
                |> adjustQueryWindow args

            args.WriteScreen state results
            <| match state.SuppressProperties with
               | true -> Ok []
               | _ -> Query.props state

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

        args.GetKey()
        |> Keys.get args.Keymaps
        |> function
            | Action.Cancel -> []
            | Action.Finish -> unwrap results
            | action ->
                action
                |> invokeAction (state |> InternalState.updateConsoleWidth (args.GetConsoleWidth())) pos context
                |||> loop args results

    let interact
        (conf: InternalConfig)
        (state: InternalState)
        (pos: Position)
        (rui: unit -> Screen.IRawUI)
        (invoke: obj list -> string seq)
        (input: Entry list)
        =

        let state, context = Query.prepare state

        let propMap =
            state.Properties |> List.map (fun p -> String.lower p, p) |> Map.ofList

        match conf.NotInteractive with
        | true ->
            let l = Query.run context input propMap
            unwrap l
        | _ ->
            use buff = Screen.init rui invoke conf.Layout

            let args =
                { Keymaps = conf.Keymaps
                  Input = input
                  PropMap = propMap
                  WriteScreen = buff.WriteScreen conf.Layout
                  GetKey = buff.GetKey
                  GetConsoleWidth = buff.GetConsoleWidth
                  GetLengthInBufferCells = buff.GetLengthInBufferCells }

            loop args input state pos context

    let buildInput acc (input: PSObject array) =
        input
        |> Seq.fold
            (fun acc o ->
                match o.BaseObject with
                | :? IDictionary as dct -> Seq.cast<DictionaryEntry> dct |> Seq.fold (fun a d -> Entry.Dict d :: a) acc
                | _ -> Entry.Obj(PSObject o) :: acc)
            acc

    let buildProperties acc (input: PSObject array) =
        Set.union acc
        <| (input
            |> Seq.collect (fun o ->
                match o.BaseObject with
                | :? IDictionary as dct ->
                    match Seq.cast<DictionaryEntry> dct with
                    | s when Seq.isEmpty s -> Seq.empty
                    | s -> s |> Seq.head |> PSObject.AsPSObject |> _.Properties
                | _ -> o.Properties)
            |> Seq.map _.Name
            |> Set.ofSeq)
