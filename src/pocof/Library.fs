namespace pocof

open System
open System.Management.Automation
open System.Management.Automation.Host
open System.Management.Automation.Runspaces
open System.Collections

open PocofData

[<Cmdlet(VerbsCommon.Select, "Pocof")>]
[<Alias("pocof")>]
[<OutputType(typeof<PSObject>)>]
type SelectPocofCommand() =
    inherit PSCmdlet()

    let mutable input: Entry list = []
    let mutable properties: Set<string> = set []

    let mutable caseSensitive: bool = false
    let mutable invertQuery: bool = false
    let mutable nonInteractive: bool = false
    let mutable suppressProperties: bool = false

    let interact
        (conf: InternalConfig)
        (state: InternalState)
        (pos: Position)
        (rui: PSHostRawUserInterface)
        (invoke: obj list -> seq<string>)
        =
        let props = List.ofSeq properties

        let pmap =
            props
            |> List.map (fun p -> String.lower p, p)
            |> Map.ofList

        match conf.NotInteractive with
        | true ->
            let _, l = PocofQuery.run state input pmap
            unwrap l
        | _ ->
            use sbf = PocofScreen.init rui conf.Prompt invoke

            let writeScreen =
                match conf.Layout with
                | TopDown -> sbf.writeTopDown
                | BottomUp -> sbf.writeBottomUp

            let getKey () =
                Async.FromContinuations(fun (cont, _, _) -> Console.ReadKey true |> cont)
                |> Async.RunSynchronously

            let rec loop (results: Entry list) (state: InternalState) (pos: Position) (refresh: Refresh) =
                let s, l =
                    match refresh with
                    | NotRequired -> state, results
                    | _ ->
                        let s, l = PocofQuery.run state input pmap

                        writeScreen s pos.X l
                        <| match state.SuppressProperties with
                           | true -> Ok []
                           | _ -> PocofQuery.props state props

                        s, l

                match PocofAction.get conf.Keymaps getKey with
                | Cancel -> []
                | Finish -> unwrap l
                | Noop -> loop l s pos NotRequired
                | a -> invokeAction s pos a |||> loop l

            loop input state pos Required

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
    member __.CaseSensitive: SwitchParameter = new SwitchParameter false

    member __.CaseSensitive
        with set (v: SwitchParameter) = caseSensitive <- v.IsPresent

    [<Parameter>]
    member __.InvertQuery: SwitchParameter = new SwitchParameter false

    member __.InvertQuery
        with set (v: SwitchParameter) = invertQuery <- v.IsPresent

    [<Parameter>]
    member __.NonInteractive: SwitchParameter = new SwitchParameter false

    member __.NonInteractive
        with set (v: SwitchParameter) = nonInteractive <- v.IsPresent

    [<Parameter>]
    member __.SuppressProperties: SwitchParameter = new SwitchParameter false

    member __.SuppressProperties
        with set (v: SwitchParameter) = suppressProperties <- v.IsPresent

    [<Parameter>]
    member val Prompt = "query" with get, set

    [<Parameter>]
    [<ValidateSet("TopDown"
      // , "BottomUp"
      )>]
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
            List.ofArray __.InputObject
            |> List.fold
                (fun acc o ->
                    match o.BaseObject with
                    | :? IDictionary as dct ->
                        Seq.cast<DictionaryEntry> dct
                        |> Seq.fold (fun a d -> Dict d :: a) acc
                    | _ -> Obj(PSObject o) :: acc)
                input

        properties <-
            __.InputObject
            |> Seq.collect (fun o ->
                match o.BaseObject with
                | :? IDictionary as dct ->
                    match Seq.cast<DictionaryEntry> dct with
                    | s when Seq.isEmpty s -> Seq.empty
                    | s ->
                        s
                        |> Seq.head
                        |> PSObject.AsPSObject
                        |> (fun o -> o.Properties)
                | _ -> o.Properties)
            |> Seq.map (fun p -> p.Name)
            |> Seq.fold (fun acc n -> acc.Add n) properties

    override __.EndProcessing() =
        input <- List.rev input

        let conf, state, pos =
            initConfig
                { Query = __.Query
                  Matcher = __.Matcher
                  Operator = __.Operator
                  CaseSensitive = caseSensitive
                  InvertQuery = invertQuery
                  NotInteractive = nonInteractive
                  SuppressProperties = suppressProperties
                  Prompt = __.Prompt
                  Layout = __.Layout
                  Keymaps = __.Keymaps |> PocofAction.convertKeymaps }

        interact conf state pos __.Host.UI.RawUI __.invoke
        |> Seq.iter __.WriteObject
