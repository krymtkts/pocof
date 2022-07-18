namespace pocof

open System
open System.Management.Automation
open System.Management.Automation.Host

[<Cmdlet(VerbsCommon.Select, "Pocof")>]
[<OutputType(typeof<PSObject>)>]
type SelectPocofCommand() =
    inherit PSCmdlet()

    let mutable input: PSObject list = []

    let interact
        (conf: PocofData.InternalConfig)
        (state: PocofData.InternalState)
        (pos: PocofData.Position)
        (rui: PSHostRawUserInterface)
        =
        use sbf = PocofScreen.init rui conf.Prompt

        let writeScreen =
            match conf.Layout with
            | PocofData.TopDown -> sbf.writeTopDown
            | PocofData.BottomUp -> sbf.writeBottomUp

        let rec loop (s: PocofData.InternalState) (p: PocofData.Position) (l: PSObject list) =
            let ns, entries = PocofQuery.run s l
            writeScreen ns p.X entries
            // TODO: should use Console.KeyAvailable?
            // if Console.KeyAvailable then
            match PocofAction.get conf.Keymaps (fun () -> Console.ReadKey true) with
            | PocofData.Cancel -> []
            | PocofData.Finish -> entries
            | a ->
                PocofData.invokeAction a ns p
                |> fun (ns, np) -> loop ns np l
        // else
        //     loop ()
        loop state pos input

    [<Parameter(Position = 0, ValueFromPipeline = true, ValueFromPipelineByPropertyName = true)>]
    member val InputObject: PSObject [] = [||] with get, set

    [<Parameter>]
    member val Query = String.Empty with get, set

    [<Parameter>]
    [<ValidateSet("match", "like", "eq")>]
    member val Matcher = PocofData.MATCH.ToString().ToLower() with get, set

    [<Parameter>]
    member val CaseSensitive = false with get, set

    [<Parameter>]
    member val InvertFilter = false with get, set

    [<Parameter>]
    member val Prompt = "query" with get, set

    [<Parameter>]
    [<ValidateSet("TopDown", "BottomUp")>]
    member val Layout = PocofData.TopDown.ToString() with get, set

    [<Parameter>]
    member val Keymaps: Collections.Hashtable = null with get, set

    override __.BeginProcessing() = base.BeginProcessing()

    override __.ProcessRecord() =
        input <- List.append input <| List.ofArray __.InputObject

    override __.EndProcessing() =
        let conf, state, pos =
            PocofData.initConfig
                { Query = __.Query
                  Matcher = __.Matcher
                  CaseSensitive = __.CaseSensitive
                  InvertFilter = __.InvertFilter
                  Prompt = __.Prompt
                  Layout = __.Layout
                  Keymaps = __.Keymaps }

        __.WriteObject
        <| interact conf state pos __.Host.UI.RawUI
