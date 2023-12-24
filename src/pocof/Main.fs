namespace pocof

open System
open System.Management.Automation.Host

open PocofData
open PocofHandle

module Pocof =
    [<TailCall>]
    let rec private read (acc: ConsoleKeyInfo list) =
        let acc = Console.ReadKey true :: acc

        match Console.KeyAvailable with
        | true -> read acc
        | _ -> List.rev acc

    let private getKey () =
        Async.FromContinuations(fun (cont, _, _) -> read [] |> cont)
        |> Async.RunSynchronously

    type LoopFixedArguments =
        { keymaps: Map<KeyPattern, Action>
          input: Entry list
          propMap: Map<string, string>
          writeScreen: PocofScreen.WriteScreen }

    [<TailCall>]
    let rec loop
        (args: LoopFixedArguments)
        (results: Entry list)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        (refresh: Refresh)
        =

        let s, l =
            match refresh with
            | NotRequired -> state, results
            | _ ->
                let s, l = PocofQuery.run state context args.input args.propMap

                args.writeScreen s pos.X l
                <| match state.SuppressProperties with
                   | true -> Ok []
                   | _ -> PocofQuery.props state

                s, l

        getKey ()
        |> PocofAction.get args.keymaps
        |> function
            | Cancel -> []
            | Finish -> unwrap l
            | Noop -> loop args l s pos context NotRequired
            | a ->
                invokeAction s pos context a
                |> fun (a, b, c, d) -> loop args l a b c d

    let interact
        (conf: InternalConfig)
        (state: InternalState)
        (pos: Position)
        (rui: PSHostRawUserInterface)
        (invoke: obj list -> seq<string>)
        (input: Entry list)
        =

        let context = PocofQuery.prepare state

        let propMap =
            state.Properties
            |> List.map (fun p -> String.lower p, p)
            |> Map.ofList

        match conf.NotInteractive with
        | true ->
            let _, l = PocofQuery.run state context input propMap
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
                    | BottomUp -> sbf.writeBottomUp }


            loop args input state pos context Required
