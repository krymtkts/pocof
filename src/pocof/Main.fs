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
          getKey: unit -> ConsoleKeyInfo list }

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

                args.writeScreen state state.QueryState.Cursor results
                <| match state.SuppressProperties with
                   | true -> Ok []
                   | _ -> PocofQuery.props state

                results

        args.getKey ()
        |> PocofAction.get args.keymaps
        |> function
            | Cancel -> []
            | Finish -> unwrap results
            | Noop -> loop args results state pos context
            | action ->
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
            use sbf = PocofScreen.init rui conf.Prompt invoke

            let args =
                { keymaps = conf.Keymaps
                  input = input
                  propMap = propMap
                  writeScreen =
                    match conf.Layout with
                    | TopDown -> sbf.writeTopDown
                    | BottomUp -> sbf.writeBottomUp
                  getKey = getKey }

            loop args input state pos context
