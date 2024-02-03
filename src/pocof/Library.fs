namespace pocof

open System
open System.Management.Automation
open System.Management.Automation.Runspaces
open System.Collections

[<Cmdlet(VerbsCommon.Select, "Pocof")>]
[<Alias("pocof")>]
[<OutputType(typeof<PSObject>)>]
type SelectPocofCommand() =
    inherit PSCmdlet()

    let mutable input: Pocof.Entry list = []
    let mutable properties: Set<string> = set []
    let mutable keymaps: Map<Pocof.KeyPattern, Pocof.Action> = Map []

    [<Parameter(Position = 0, ValueFromPipeline = true, ValueFromPipelineByPropertyName = true)>]
    member val InputObject: PSObject [] = [||] with get, set

    [<Parameter>]
    member val Query = String.Empty with get, set

    [<Parameter>]
    [<ValidateSet("match", "like", "eq")>]
    member val Matcher = string Pocof.Matcher.MATCH with get, set

    [<Parameter>]
    [<ValidateSet("and", "or", "none")>]
    member val Operator = string Pocof.Operator.AND with get, set

    [<Parameter>]
    member val CaseSensitive: SwitchParameter = new SwitchParameter false with get, set

    [<Parameter>]
    member val InvertQuery: SwitchParameter = new SwitchParameter false with get, set

    [<Parameter>]
    member val NonInteractive: SwitchParameter = new SwitchParameter false with get, set

    [<Parameter>]
    member val SuppressProperties: SwitchParameter = new SwitchParameter false with get, set

    [<Parameter>]
    member val Prompt = "query" with get, set

    [<Parameter>]
    [<ValidateSet("TopDown", "TopDownHalf", "BottomUp", "BottomUpHalf")>]
    member val Layout = string Pocof.Layout.TopDown with get, set

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

    override __.BeginProcessing() =
        match Pocof.convertKeymaps __.Keymaps with
        | Ok k -> keymaps <- k
        | Error e -> new ArgumentException(e) |> raise

    override __.ProcessRecord() =
        input <- __.InputObject |> Pocof.buildInput input

        properties <- __.InputObject |> Pocof.buildProperties properties

    override __.EndProcessing() =
        let conf, state, pos =
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
                  Properties = List.ofSeq properties
                  EntryCount = input |> List.length
                  ConsoleWidth = __.Host.UI.RawUI.WindowSize.Width
                  ConsoleHeight = __.Host.UI.RawUI.WindowSize.Height }

        Pocof.interact conf state pos
        <| fun _ -> new Pocof.RawUI(__.Host.UI.RawUI, conf.Layout)
        <| __.invoke
        <| List.rev input
        |> Seq.iter __.WriteObject
