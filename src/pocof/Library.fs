namespace Pocof

open System
open System.Collections
open System.Management.Automation
open System.Management.Automation.Host
open System.Management.Automation.Runspaces
open System.Reflection

[<Cmdlet(VerbsCommon.Select, "Pocof")>]
[<Alias("pocof")>]
[<OutputType(typeof<PSObject>)>]
type SelectPocofCommand() =
    inherit PSCmdlet()

    let mutable input: Pocof.IInputStore = Pocof.NormalInputStore()
    let properties: Pocof.PropertyStore = Pocof.PropertyStore()
    let handler: Pocof.RenderHandler = Pocof.RenderHandler()
    let mutable keymaps: Map<Pocof.KeyPattern, Pocof.Action> = Map []
    let mutable renderPeriodic: unit -> unit = fun () -> ()
    let mutable waitResult: Pocof.Termination -> obj seq = fun (_) -> Seq.empty

    [<Parameter(ValueFromPipeline = true, ValueFromPipelineByPropertyName = true)>]
    [<ValidateNotNull>]
    member val InputObject: PSObject[] = [||] with get, set

    [<Parameter(Position = 0)>]
    [<ValidateNotNull>]
    member val Query = String.Empty with get, set

    [<Parameter>]
    [<ValidateSet("Match", "Like", "Eq")>]
    member val Matcher = string Pocof.Matcher.Match with get, set

    [<Parameter>]
    [<ValidateSet("And", "Or")>]
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
    [<ValidateNotNull>]
    member val Prompt = "query" with get, set

    [<Parameter>]
    [<ValidateSet("TopDown", "TopDownHalf", "BottomUp", "BottomUpHalf")>]
    member val Layout = string Pocof.Layout.TopDown with get, set

    [<Parameter>]
    member val Keymaps: Hashtable | null = null with get, set

    [<Parameter>]
    [<ValidateNotNullOrEmpty>]
    member val WordDelimiters = Pocof.defaultWordDelimiters with get, set

    abstract member Invoke: 'a seq -> string seq

    default __.Invoke(input: 'a seq) =
        __.InvokeCommand.InvokeScript(
            @"$input | Format-Table | Out-String",
            true,
            PipelineResultTypes.None,
            Array.ofSeq input,
            null
        )
        |> Seq.map string

    abstract member PSHost: unit -> PSHost
    default __.PSHost() = __.Host

    abstract member ConsoleInterface: unit -> Pocof.IConsoleInterface
    default __.ConsoleInterface() = Pocof.initConsoleInterface ()

    abstract member GetStopUpstreamCommandsExceptionType: unit -> Type

    default __.GetStopUpstreamCommandsExceptionType() =
        Assembly.GetAssembly(typeof<PSCmdlet>).GetType("System.Management.Automation.StopUpstreamCommandsException")

    override __.BeginProcessing() =
        match Pocof.convertKeymaps __.Keymaps with
        | Ok k -> keymaps <- k
        | Error e -> ArgumentException(e) |> raise

        input <- __.Unique.IsPresent |> Pocof.getInputStore

        let cancelAction () =
            // NOTE: required to call EndProcessing manually when the upstream command is stopped.
            __.ForceEndProcessing()

            __
            |> Pocof.stopUpstreamCommandsException (__.GetStopUpstreamCommandsExceptionType())
            |> raise

        let r, w =
            Pocof.init
                (__.PSHost().UI.RawUI)
                (__.ConsoleInterface())
                __.Invoke
                cancelAction
                handler
                input.GetEntries
                { Query = __.Query
                  Matcher = __.Matcher
                  Operator = __.Operator
                  CaseSensitive = __.CaseSensitive.IsPresent
                  InvertQuery = __.InvertQuery.IsPresent
                  NotInteractive = __.NonInteractive.IsPresent
                  SuppressProperties = __.SuppressProperties.IsPresent
                  Prompt = __.Prompt
                  WordDelimiters = __.WordDelimiters
                  Layout = __.Layout
                  Keymaps = keymaps
                  Properties = properties.GetProperties()
                  PropertiesMap = properties.GetPropertyMap()
                  ConsoleWidth = __.PSHost().UI.RawUI.WindowSize.Width
                  ConsoleHeight = __.PSHost().UI.RawUI.WindowSize.Height }

        renderPeriodic <- r
        waitResult <- w

    member private __.ProcessTermination(term: Pocof.Termination) =
        waitResult term |> Seq.iter __.WriteObject

    // NOTE: Unfortunately, EndProcessing is protected method in PSCmdlet. so we cannot use it publicly.
    member internal __.ForceEndProcessing() =
        Pocof.Termination.Force |> __.ProcessTermination

    override __.ProcessRecord() =
        for o in __.InputObject do
            o |> input.Add
            o |> Pocof.buildProperties properties.ContainsKey properties.Add

        renderPeriodic ()

    override __.EndProcessing() =
        Pocof.Termination.Normal |> __.ProcessTermination
