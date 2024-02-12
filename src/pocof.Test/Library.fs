module PocofTest.Library

module Mock =
    open System
    open System.Management.Automation
    open System.Management.Automation.Host

    type MyRawUI() =
        inherit PSHostRawUserInterface()
        override val ForegroundColor = ConsoleColor.White with get, set
        override val BackgroundColor = ConsoleColor.Black with get, set
        override val CursorPosition = new Coordinates(0, 0) with get, set
        override val WindowPosition = new Coordinates(0, 0) with get, set
        override val CursorSize = 1 with get, set
        override val BufferSize = new Size(80, 25) with get, set
        override val WindowSize = new Size(80, 25) with get, set
        override val MaxWindowSize = new Size(80, 25)
        override val MaxPhysicalWindowSize = new Size(80, 25)
        override __.ReadKey(_) = KeyInfo()
        override __.FlushInputBuffer() = ()
        override val KeyAvailable = true
        override val WindowTitle = "" with get, set
        override __.SetBufferContents(origin: Coordinates, contents: BufferCell array2d) = ()
        override __.SetBufferContents(rectangle: Rectangle, fill: BufferCell) = ()

        override __.GetBufferContents(rectangle: Rectangle) =
            Array2D.init 1 1 (fun _ _ -> BufferCell())

        override __.ScrollBufferContents
            (
                source: Rectangle,
                destination: Coordinates,
                clip: Rectangle,
                fill: BufferCell
            ) =
            ()

    type MyUI() =
        inherit PSHostUserInterface()
        override __.RawUI: PSHostRawUserInterface = new MyRawUI()
        override __.ReadLine() = ""
        override __.ReadLineAsSecureString() = null
        override __.Write(value: string) = ()

        override __.Write(foregroundColor: System.ConsoleColor, backgroundColor: System.ConsoleColor, value: string) =
            ()

        override __.WriteLine(value: string) = ()
        override __.WriteErrorLine(value: string) = ()
        override __.WriteDebugLine(_) = ()
        override __.WriteProgress(_, _) = ()
        override __.WriteVerboseLine(_) = ()

        override __.WriteWarningLine(_) = ()
        override __.Prompt(_, _, _) = null
        override __.PromptForCredential(_, _, _, _) = null
        override __.PromptForCredential(_, _, _, _, _, _) = null
        override __.PromptForChoice(_, _, _, _) = 0

    type MyHost() =
        inherit PSHost()

        override __.Name: string = "MyHost"
        override __.Version = new Version(1, 0, 0, 0)
        override __.InstanceId = Guid.NewGuid()
        override __.UI = new MyUI()
        override __.CurrentCulture = Globalization.CultureInfo.InvariantCulture
        override __.CurrentUICulture = Globalization.CultureInfo.InvariantCulture
        override __.SetShouldExit(exitCode: int) = ()
        override __.EnterNestedPrompt() = ()
        override __.ExitNestedPrompt() = ()
        override __.NotifyBeginApplication() = ()
        override __.NotifyEndApplication() = ()


    type CommandRuntime() =
        let mutable output: string list = List.empty
        let mutable errors: ErrorRecord list = List.empty
        let mutable warnings: string list = List.empty

        member __.Output = output
        member __.Errors = errors
        member __.Warnings = warnings

        interface ICommandRuntime with
            member __.Host: PSHost = new MyHost()
            member __.CurrentPSTransaction = null
            member __.WriteDebug(_) = ()
            member __.WriteError(errorRecord: ErrorRecord) = errors <- errorRecord :: errors
            member __.WriteObject(obj: obj) = output <- obj.ToString() :: output
            member __.WriteObject(obj: obj, _) = output <- obj.ToString() :: output
            member __.WriteProgress _ = ()
            member __.WriteProgress(_, _) = ()
            member __.WriteVerbose(_) = ()
            member __.WriteWarning(warning: string) = warnings <- warning :: warnings

            member __.WriteCommandDetail(_) = ()
            member __.ShouldProcess(_) = true
            member __.ShouldProcess(_, _) = true
            member __.ShouldProcess(_, _, _) = true
            member __.ShouldProcess(_, _, _, _) = true
            member __.ShouldContinue(_, _) = true
            member __.ShouldContinue(_, _, _, _) = true
            member __.TransactionAvailable() = true
            member __.ThrowTerminatingError(errorRecord: ErrorRecord) = errors <- errorRecord :: errors

module SelectPocofCommand =
    open System
    open System.Collections
    open System.Management.Automation
    open System.Management.Automation.Host

    open Xunit
    open FsUnitTyped

    open Pocof

    type SelectPocofCommandForTest() =
        inherit SelectPocofCommand()

        member val Host: PSHost = new Mock.MyHost()
        override __.invoke(input: 'a list) = input |> Seq.map string
        override __.host() = __.Host

        member __.InvokeForTest() =
            __.BeginProcessing()
            __.ProcessRecord()
            __.EndProcessing()

    [<Fact>]
    let ``should return values with non-interactive mode.`` () =
        let runtime = new Mock.CommandRuntime()
        let cmdlet = new SelectPocofCommandForTest()

        cmdlet.CommandRuntime <- runtime
        cmdlet.InputObject <- [| PSObject.AsPSObject "a" |]
        cmdlet.NonInteractive <- true
        cmdlet.InvokeForTest()

        runtime.Output |> shouldEqual [ "a" ]

    [<Fact>]
    let ``should raise ArgumentException when invalid keymaps.`` () =
        let runtime = new Mock.CommandRuntime()
        let cmdlet = new SelectPocofCommandForTest()

        cmdlet.CommandRuntime <- runtime
        cmdlet.InputObject <- [| PSObject.AsPSObject "a" |]

        cmdlet.Keymaps <-
            let k = new Hashtable()
            k.Add("Escape", "cancellation")
            k

        shouldFail<ArgumentException> (fun () -> cmdlet.InvokeForTest())
