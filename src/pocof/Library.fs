namespace pocof

open System
open System.Management.Automation
open System.Management.Automation.Host
open System.Management.Automation.Runspaces
open System.Collections

open PocofData

module Pocof =
    [<TailCall>]
    let rec read (acc: ConsoleKeyInfo list) =
        let acc = Console.ReadKey true :: acc

        match Console.KeyAvailable with
        | true -> read acc
        | _ -> List.rev acc

    let getKey () =
        Async.FromContinuations(fun (cont, _, _) -> read [] |> cont)
        |> Async.RunSynchronously

    type LoopFixedArguments = {
        keymaps: Map<KeyPattern,Action>
        input: Entry list
        props: string list
        propMap: Map<string,string>
        writeScreen: PocofScreen.WriteScreen
    }

    [<TailCall>]
    let rec loop
        (args: LoopFixedArguments)
        (results: Entry list)
        (state: InternalState)
        (pos: Position)
        (refresh: Refresh)
        =

        let s, l =
            match refresh with
            | NotRequired -> state, results
            | _ ->
                let s, l = PocofQuery.run state args.input args.propMap

                args.writeScreen s pos.X l
                <| match state.SuppressProperties with
                   | true -> Ok []
                   | _ -> PocofQuery.props state args.props

                s, l

        getKey ()
        |> PocofAction.get args.keymaps
        |> function
           | Cancel -> []
           | Finish -> unwrap l
           | Noop -> loop args l s pos NotRequired
           | a -> invokeAction s pos args.props a |||> loop args l

    let interact
        (conf: InternalConfig)
        (state: InternalState)
        (pos: Position)
        (rui: PSHostRawUserInterface)
        (invoke: obj list -> seq<string>)
        (input: Entry list)
        (props: string list)
        =

        let propMap =
            props
            |> List.map (fun p -> String.lower p, p)
            |> Map.ofList

        match conf.NotInteractive with
        | true ->
            let _, l = PocofQuery.run state input propMap
            unwrap l
        | _ ->
            use sbf = PocofScreen.init rui conf.Prompt invoke

            let args = {
                keymaps = conf.Keymaps
                input = input
                props = props
                propMap = propMap
                writeScreen =
                    match conf.Layout with
                    | TopDown -> sbf.writeTopDown
                    | BottomUp -> sbf.writeBottomUp
            }

            loop args input state pos Required

[<Cmdlet(VerbsCommon.Select, "Pocof")>]
[<Alias("pocof")>]
[<OutputType(typeof<PSObject>)>]
type SelectPocofCommand() =
    inherit PSCmdlet()

    let mutable input: Entry list = []
    let mutable properties: Set<string> = set []

    [<Parameter(Position = 0, ValueFromPipeline = true, ValueFromPipelineByPropertyName = true)>]
    member val InputObject: PSObject [] = [||] with get, set

    [<Parameter>]
    member val Query = String.Empty with get, set

    [<Parameter>]
    [<ValidateSet("match", "like", "eq")>]
    member val Matcher = string MATCH with get, set

    [<Parameter>]
    [<ValidateSet("and", "or", "none")>]
    member val Operator = string AND with get, set

    [<Parameter>]
    member val CaseSensitive: SwitchParameter = new SwitchParameter false with get, set

    [<Parameter>]
    member val InvertQuery: SwitchParameter = new SwitchParameter false with get,set

    [<Parameter>]
    member val NonInteractive: SwitchParameter = new SwitchParameter false with get,set

    [<Parameter>]
    member val SuppressProperties: SwitchParameter = new SwitchParameter false with get,set

    [<Parameter>]
    member val Prompt = "query" with get, set

    [<Parameter>]
    [<ValidateSet("TopDown", "BottomUp")>]
    member val Layout = string TopDown with get, set

    [<Parameter>]
    member val Keymaps: Hashtable = null with get, set

    member __.invoke input =
        __.InvokeCommand.InvokeScript(
            @"$input | Format-Table | Out-String",
            true,
            PipelineResultTypes.None,
            Array.ofList (input),
            null
        )
        |> Seq.map string

    override __.BeginProcessing() = base.BeginProcessing()

    override __.ProcessRecord() =
        input <-
            __.InputObject
            |> List.ofArray
            |> List.fold
                (fun acc o ->
                    match o.BaseObject with
                    | :? IDictionary as dct ->
                        Seq.cast<DictionaryEntry> dct
                        |> Seq.fold (fun a d -> Dict d :: a) acc
                    | _ -> Obj(PSObject o) :: acc)
                input

        properties <-
            Set.union properties
            <| (__.InputObject
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

    override __.EndProcessing() =
        let conf, state, pos =
            initConfig
                { Query = __.Query
                  Matcher = __.Matcher
                  Operator = __.Operator
                  CaseSensitive = __.CaseSensitive.IsPresent
                  InvertQuery = __.InvertQuery.IsPresent
                  NotInteractive = __.NonInteractive.IsPresent
                  SuppressProperties = __.SuppressProperties.IsPresent
                  Prompt = __.Prompt
                  Layout = __.Layout
                  Keymaps = __.Keymaps |> PocofAction.convertKeymaps }

        Pocof.interact conf state pos __.Host.UI.RawUI __.invoke <| List.rev input <| List.ofSeq properties
        |> Seq.iter __.WriteObject
