namespace Pocof

open System
open System.Collections
open System.Management.Automation
open System.Management.Automation.Host
open System.Management.Automation.Runspaces
open System.Threading.Tasks

[<Cmdlet(VerbsCommon.Select, "Pocof")>]
[<Alias("pocof")>]
[<OutputType(typeof<PSObject>)>]
type SelectPocofCommand() =
    inherit PSCmdlet()

    let mutable input: Pocof.IInputStore = Pocof.NormalInputStore()
    let properties: Pocof.PropertyStore = Pocof.PropertyStore()
    let handler: Pocof.RenderHandler = Pocof.RenderHandler()
    let mutable keymaps: Map<Pocof.KeyPattern, Pocof.Action> = Map []
    let mutable buff: Pocof.Buff option = None
    let mutable mainTask: obj seq Task option = None
    let mutable conf: Pocof.InternalConfig option = None
    let mutable periodic: Pocof.Periodic option = None

    [<Parameter(Position = 0, ValueFromPipeline = true, ValueFromPipelineByPropertyName = true)>]
    member val InputObject: PSObject[] = [||] with get, set

    [<Parameter>]
    member val Query = String.Empty with get, set

    [<Parameter>]
    [<ValidateSet("Match", "Like", "Eq")>]
    member val Matcher = string Pocof.Matcher.Match with get, set

    [<Parameter>]
    [<ValidateSet("And", "Or", "None")>]
    member val Operator = string Pocof.Operator.And with get, set

    [<Parameter>]
    member val CaseSensitive: SwitchParameter = SwitchParameter false with get, set

    [<Parameter>]
    member val InvertQuery: SwitchParameter = SwitchParameter false with get, set

    [<Parameter>]
    member val NonInteractive: SwitchParameter = SwitchParameter false with get, set

    [<Parameter>]
    member val SuppressProperties: SwitchParameter = SwitchParameter false with get, set

    [<Parameter>]
    member val Unique: SwitchParameter = SwitchParameter false with get, set

    [<Parameter>]
    member val Prompt = "query" with get, set

    [<Parameter>]
    [<ValidateSet("TopDown", "TopDownHalf", "BottomUp", "BottomUpHalf")>]
    member val Layout = string Pocof.Layout.TopDown with get, set

    [<Parameter>]
    member val Keymaps: Hashtable = null with get, set

    abstract member Invoke: 'a seq -> string seq

    default __.Invoke(input: 'a seq) =
        __.InvokeCommand.InvokeScript(
            @"$input | Format-Table | Out-String",
            true,
            PipelineResultTypes.None,
            Array.ofSeq (input),
            null
        )
        |> Seq.map string

    abstract member PSHost: unit -> PSHost
    default __.PSHost() = __.Host

    // member __.ForceQuit() = handler.Publish(Pocof.RenderEvent.Quit)

    override __.BeginProcessing() =
        match Pocof.convertKeymaps __.Keymaps with
        | Ok k -> keymaps <- k
        | Error e -> ArgumentException(e) |> raise

        input <- __.Unique.IsPresent |> Pocof.getInputStore

        let cnf, state, pos =
            Pocof.initConfig
                { Query = __.Query
                  Matcher = __.Matcher
                  Operator = __.Operator
                  CaseSensitive = __.CaseSensitive.IsPresent
                  InvertQuery = __.InvertQuery.IsPresent
                  NotInteractive = __.NonInteractive.IsPresent
                  SuppressProperties = __.SuppressProperties.IsPresent
                  Prompt = __.Prompt
                  Layout = __.Layout
                  Keymaps = keymaps
                  Properties = properties.GetAll()
                  EntryCount = input.Count()
                  ConsoleWidth = __.PSHost().UI.RawUI.WindowSize.Width
                  ConsoleHeight = __.PSHost().UI.RawUI.WindowSize.Height }

        conf <- cnf |> Some
        buff <- Pocof.initScreen (fun _ -> new Pocof.RawUI(__.PSHost().UI.RawUI)) __.Invoke cnf

        mainTask <-
            async { return Pocof.interact cnf state pos buff handler.Publish <| input.GetAll() }
            |> Async.StartAsTask
            |> Some

        periodic <-
            match buff with
            | None -> None
            | Some buff -> Pocof.Periodic(cnf, handler, buff) |> Some

    // NOTE: Unfortunately, EndProcessing is protected method in PSCmdlet. so we cannot use it publicly.
    member internal __.ForceEndProcessing() = __.EndProcessing()

    override __.ProcessRecord() =
        for o in __.InputObject do
            o |> input.Add
            o |> Pocof.buildProperties properties.ContainsKey properties.Add

        periodic
        |> Option.iter (fun interval ->
            interval.Render(fun _ ->
                // NOTE: to disable the interactive mode at EndProcessing.
                conf <- None
                // NOTE: required to call EndProcessing manually when the upstream command is stopped.
                __.ForceEndProcessing()
                __ |> Pocof.stopUpstreamCommandsException |> raise))

    override __.EndProcessing() =
        periodic |> Option.iter _.Stop()
        conf |> Option.iter (fun conf -> buff |> Pocof.render conf handler)
        buff |> Option.dispose
        mainTask |> Option.iter (_.Result >> Seq.iter __.WriteObject)
