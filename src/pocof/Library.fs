namespace pocof

open System
open System.Management.Automation // PowerShell attributes come from this namespace

module ScreenBuffer =
    type Buff =
        val rui: PSHostRawUserInterface
        val buf: BufferCell [,]
        new(r, b) = { rui = r; buf = b }

type ScreenBufferBuilder() =
    member _.TryFinally(body, compensation) =
        try
            printfn "TryFinally Body"
            body ()
        finally
            printfn "TryFinally compensation"
            compensation ()

    member __.Using(disposable: #IDisposable, body) =
        let body' = fun () -> body disposable

        __.TryFinally(
            body',
            fun () ->
                match disposable with
                | null -> ()
                | disp -> disp.Dispose()
        )


/// Describe cmdlet in /// comments
/// Cmdlet attribute takes verb names as strings or verb enums
/// Output type works the same as for PowerShell cmdlets
[<Cmdlet(VerbsCommon.Select, "Pocof")>]
[<OutputType(typeof<string>)>]
type SelectPocofCommand() =
    inherit PSCmdlet()

    // // cmdlet parameters are properties of the class
    // let screenbuffer = new ScreenBufferBuilder()
    let mutable input: Object list = []


    /// Describe property params in /// comments
    /// Parameter, Validate, and Alias attributes work the same as PowerShell params
    [<Parameter(Position = 0, ValueFromPipeline = true, ValueFromPipelineByPropertyName = true)>]
    member val InputObject: Object [] = [||] with get, set

    [<Parameter>]
    member val Query = String.Empty with get, set

    [<Parameter>]
    [<ValidateSet("match", "like", "eq")>]
    member val Filter = PocofData.MATCH.ToString().ToLower() with get, set

    [<Parameter>]
    member val CaseSensitive = false with get, set

    [<Parameter>]
    member val InvertFilter = false with get, set

    [<Parameter>]
    member val Prompt = String.Empty with get, set

    [<Parameter>]
    [<ValidateSet("TopDown", "BottomUp")>]
    member val Layout = PocofData.TopDown.ToString() with get, set

    [<Parameter>]
    member val Keymaps: Collections.Hashtable = null with get, set

    // optional: handle each pipeline value (e.g. InputObject)
    override __.ProcessRecord() =
        input <- List.append input <| List.ofArray __.InputObject

    // optional: finish after all pipeline input
    override __.EndProcessing() =
        use sbf = PocofScreen.init __.Host.UI.RawUI

        let conf, state, pos =
            PocofData.initConfig
                { Query = __.Query
                  Filter = __.Filter
                  CaseSensitive = __.CaseSensitive
                  InvertFilter = __.InvertFilter
                  Prompt = __.Prompt
                  Layout = __.Layout
                  Keymaps = __.Keymaps }

        let writeScreen =
            match conf.Layout with
            | PocofData.TopDown -> sbf.writeTopDown
            | PocofData.BottomUp -> sbf.writeBottomUp

        let rec loop () =
            let entries = input // TODO: filter this with LINQ.
            writeScreen conf.Prompt state.Query entries
            // TODO: should use Console.KeyAvailable?
            // if Console.KeyAvailable then
            match PocofAction.get conf.Keymaps (fun () -> Console.ReadKey true) with
            | PocofData.Cancel -> ()
            | PocofData.Finish -> __.WriteObject input
            | _ -> loop ()
        // else
        //     loop ()

        loop ()
