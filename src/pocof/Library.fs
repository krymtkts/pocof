namespace Pocof

open System
open System.Collections
open System.Management.Automation
open System.Management.Automation.Host
open System.Management.Automation.Runspaces

[<Cmdlet(VerbsCommon.Select, "Pocof")>]
[<Alias("pocof")>]
[<OutputType(typeof<PSObject>)>]
type SelectPocofCommand() =
    inherit PSCmdlet()

    let mutable input: Pocof.IInputStore = Pocof.NormalInputStore()
    let properties: Generic.Dictionary<string, string seq> = Generic.Dictionary()
    let mutable keymaps: Map<Pocof.KeyPattern, Pocof.Action> = Map []

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

    override __.BeginProcessing() =
        match Pocof.convertKeymaps __.Keymaps with
        | Ok k -> keymaps <- k
        | Error e -> ArgumentException(e) |> raise

        input <- __.Unique.IsPresent |> Pocof.getInputStore

    override __.ProcessRecord() =
        for o in __.InputObject do
            o |> input.Add
            o |> Pocof.buildProperties properties.ContainsKey properties.Add

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
                  Properties = properties.Values |> Seq.concat |> Set.ofSeq |> Seq.toList
                  EntryCount = input.Count()
                  ConsoleWidth = __.PSHost().UI.RawUI.WindowSize.Width
                  ConsoleHeight = __.PSHost().UI.RawUI.WindowSize.Height }

        let buff =
            Pocof.initScreen (fun _ -> new Pocof.RawUI(__.PSHost().UI.RawUI)) __.Invoke conf

        Pocof.interact conf state pos buff <| input.GetAll() |> Seq.iter __.WriteObject

        buff |> Option.dispose
