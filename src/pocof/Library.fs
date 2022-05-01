namespace pocof

open System
open System.Management.Automation // PowerShell attributes come from this namespace

/// Describe cmdlet in /// comments
/// Cmdlet attribute takes verb names as strings or verb enums
/// Output type works the same as for PowerShell cmdlets
[<Cmdlet(VerbsCommon.Select, "Pocof")>]
[<OutputType(typeof<string>)>]
type SelectPocofCommand() =
    inherit PSCmdlet()

    // cmdlet parameters are properties of the class

    /// Describe property params in /// comments
    /// Parameter, Validate, and Alias attributes work the same as PowerShell params
    [<Parameter(Position = 0, ValueFromPipeline = true, ValueFromPipelineByPropertyName = true)>]
    member val InputObject: obj [] = [||] with get, set

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
    override x.BeginProcessing() = base.BeginProcessing()

    // optional: handle each pipeline value (e.g. InputObject)
    override x.ProcessRecord() = base.ProcessRecord()

    // optional: finish after all pipeline input
    override x.EndProcessing() =
        x.InputObject
        |> Array.map (fun x -> x.ToString())
        |> String.concat ","
        |> printfn "Hello %s"

        x.WriteObject x.InputObject
        base.EndProcessing()
