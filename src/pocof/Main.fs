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

    [<TailCall>]
    let rec loop
        (args: LoopFixedArguments)
        (results: Entry list)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =

        let results =
            match state.Refresh with
            | NotRequired -> results
            | _ ->
                let results = PocofQuery.run context args.input args.propMap

                args.writeScreen state results
                <| match state.SuppressProperties with
                   | true -> Ok []
                   | _ -> PocofQuery.props state

                results

        let updateConsoleWidth (state: InternalState) =
            { state with ConsoleWidth = args.getConsoleWidth () }
            |> InternalState.updateWindowWidth

        args.getKey ()
        |> PocofAction.get args.keymaps
        |> function
            | Cancel -> []
            | Finish -> unwrap results
            | Noop ->
                let state = updateConsoleWidth state

                loop args results state pos context
            | action ->
                let state = updateConsoleWidth state

                invokeAction state pos context action
                |||> loop args results

    [<TailCall>]
    let rec private read (acc: ConsoleKeyInfo list) =
        let acc = Console.ReadKey true :: acc

        match Console.KeyAvailable with
        | true -> read acc
        | _ -> List.rev acc

    let private getKey () =
        Async.FromContinuations(fun (cont, _, _) -> read [] |> cont)
        |> Async.RunSynchronously

    let private getConsoleWidth () = Console.WindowWidth

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
            use sbf = PocofScreen.init rui invoke

            let args =
                { keymaps = conf.Keymaps
                  input = input
                  propMap = propMap
                  writeScreen =
                    match conf.Layout with
                    | TopDown -> sbf.writeTopDown
                    | BottomUp -> sbf.writeBottomUp
                  getKey = getKey
                  getConsoleWidth = getConsoleWidth }

            loop args input state pos context
