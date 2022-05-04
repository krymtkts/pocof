namespace pocof

open System
open System.Management.Automation // PowerShell attributes come from this namespace
open System.Management.Automation.Host // PowerShell attributes come from this namespace

module ScreenBuffer =
    type Buff =
        val rui: PSHostRawUserInterface
        val buf: BufferCell [,]
        new(r, b) = { rui = r; buf = b }

        interface IDisposable with
            member __.Dispose() =
                Console.Clear()
                let origin = Coordinates(0, 0)
                __.rui.SetBufferContents(origin, __.buf)
                __.rui.CursorPosition <- Coordinates(0, __.buf.GetUpperBound(0))

    let init (rui: PSHostRawUserInterface) =
        let rect = new Rectangle(0, 0, rui.WindowSize.Width, rui.CursorPosition.Y)
        let buf = new Buff(rui, rui.GetBufferContents(rect))
        Console.Clear()
        buf

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
    member val Filter = String.Empty with get, set

    [<Parameter>]
    member val CaseSensitive = false with get, set

    [<Parameter>]
    member val InvertFilter = false with get, set

    [<Parameter>]
    member val Prompt = String.Empty with get, set

    [<Parameter>]
    [<ValidateSet("TopDown", "BottomUp")>]
    member val Layout = String.Empty with get, set

    [<Parameter>]
    member val Keymaps: Collections.Hashtable = null with get, set


    // optional: setup before pipeline input starts (e.g. Name is set, InputObject is not)
    override __.BeginProcessing() =
        let now = DateTime.Now.ToString "yyyy-MM-dd hh:mm:ss.fffff"
        printfn "begin %s\n" now

    // optional: handle each pipeline value (e.g. InputObject)
    override __.ProcessRecord() =
        let now = DateTime.Now.ToString "yyyy-MM-dd hh:mm:ss.fffff"

        let prin x = printfn "Processing %s %s\n" x now

        __.InputObject
        |> Array.map (fun x -> x.ToString())
        |> String.concat ","
        |> prin

        input <- List.append input <| List.ofArray __.InputObject

    // optional: finish after all pipeline input
    override __.EndProcessing() =
        let now = DateTime.Now.ToString("yyyy-MM-dd hh:mm:ss.fffff")
        printfn "end %s\n" now

        use sbf = ScreenBuffer.init __.Host.UI.RawUI

        let rec loop () =
            // TODO: should use Console.KeyAvailable?
            match PocofAction.get () with
            | PocofAction.Cancel
            | PocofAction.Finish -> ()
            | _ -> loop ()

        loop ()

        __.WriteObject __.InputObject
