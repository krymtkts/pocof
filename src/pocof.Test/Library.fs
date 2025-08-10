module PocofTest.Library

module Mock =
    open System
    open System.Management.Automation
    open System.Management.Automation.Host

    type RawUI() =
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
            (source: Rectangle, destination: Coordinates, clip: Rectangle, fill: BufferCell)
            =
            ()

    type UI() =
        inherit PSHostUserInterface()
        override __.RawUI: PSHostRawUserInterface = new RawUI()
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

    type Host() =
        inherit PSHost()

        override __.Name: string = "MyHost"
        override __.Version = new Version(1, 0, 0, 0)
        override __.InstanceId = Guid.NewGuid()
        override __.UI = new UI()
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

        let write (obj: obj | null) =
            match obj with
            | null -> failwith "null"
            | obj -> output <- (nullArgCheck "obj" (obj.ToString())) :: output

        member __.Output = output
        member __.Errors = errors
        member __.Warnings = warnings

        interface ICommandRuntime with
            member __.Host: PSHost = new Host()
            member __.CurrentPSTransaction = null
            member __.WriteDebug(_) = ()
            member __.WriteError(errorRecord: ErrorRecord) = errors <- errorRecord :: errors

            member __.WriteObject(obj: obj) = write obj

            member __.WriteObject(obj: obj, _) = write obj

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

    open Expecto
    open Expecto.Flip

    open Pocof
    open System.Threading

    // NOTE: instead of the StopUpstreamCommandsException.
    type MockException(o: obj) =
        inherit Exception()

    type MockConsoleInterface() =
        let mutable keyAvailable = true

        interface Pocof.IConsoleInterface with

            member __.ReadKey(_: bool) =
                keyAvailable <- false
                Async.Sleep 100 |> Async.RunSynchronously
                new ConsoleKeyInfo('\000', ConsoleKey.Escape, false, false, false)

            member __.Write(s: string) = ()
            member __.WriteLine() = ()

            member __.TreatControlCAsInput
                with get () = true
                and set (v: bool): unit = ()

            member __.CursorVisible
                with get () = true
                and set (v: bool): unit = ()

            member __.KeyAvailable = keyAvailable

    type SelectPocofCommandForTest() =
        inherit SelectPocofCommand()

        member val Host: PSHost = new Mock.Host()
        override __.Invoke(input: 'a seq) = input |> Seq.map string
        override __.PSHost() = __.Host

        override __.ConsoleInterface() = new MockConsoleInterface()

        override __.GetStopUpstreamCommandsExceptionType() = typeof<MockException>

        // NOTE: PSCmdlet cannot invoke directly. So, use this method for testing.
        member __.InvokeForTest() =
            __.BeginProcessing()
            __.ProcessRecord()
            __.EndProcessing()

        member __.InvokeForTerminationTest() =
            __.BeginProcessing()

            while true do
                Thread.Sleep 100
                __.ProcessRecord()

    [<Tests>]
    let tests =
        testList
            "SelectPocofCommand"
            [

              test "When non-interactive mode, should return values" {
                  let runtime = new Mock.CommandRuntime()
                  let cmdlet = SelectPocofCommandForTest()
                  cmdlet.CommandRuntime <- runtime
                  cmdlet.InputObject <- [| PSObject.AsPSObject "a" |]
                  cmdlet.NonInteractive <- true
                  cmdlet.InvokeForTest()

                  runtime.Output
                  |> Expect.equal "should return values with non-interactive mode" [ "a" ]
              }

              test "When invalid keymaps, should raise ArgumentException" {
                  let runtime = new Mock.CommandRuntime()
                  let cmdlet = new SelectPocofCommandForTest()
                  cmdlet.CommandRuntime <- runtime
                  cmdlet.InputObject <- [| PSObject.AsPSObject "a" |]

                  cmdlet.Keymaps <-
                      let k = new Hashtable()
                      k.Add("Escape", "cancellation")
                      k

                  Expect.throwsT<ArgumentException> "should raise ArgumentException when invalid keymaps" (fun () ->
                      cmdlet.InvokeForTest())
              }

              test "When cancellation received, should return" {
                  let runtime = new Mock.CommandRuntime()
                  let cmdlet = new SelectPocofCommandForTest()
                  cmdlet.CommandRuntime <- runtime
                  cmdlet.InputObject <- [| PSObject.AsPSObject "a" |]

                  cmdlet.Keymaps <-
                      let k = new Hashtable()
                      k.Add("Escape", "Cancel")
                      k

                  Expect.throwsT<MockException> "should raise when cancellation received" (fun () ->
                      cmdlet.InvokeForTerminationTest())

              }

              test "When Escape is Finish, should return values" {
                  let runtime = new Mock.CommandRuntime()
                  let cmdlet = new SelectPocofCommandForTest()

                  cmdlet.CommandRuntime <- runtime
                  cmdlet.InputObject <- [| PSObject.AsPSObject "a" |]

                  cmdlet.Keymaps <-
                      let k = new Hashtable()
                      k.Add("Escape", "Finish")
                      k

                  cmdlet.InvokeForTest()

                  runtime.Output
                  |> Expect.equal "should return values when Escape is Finish" [ "a" ]
              }

              test "When setting properties, should reflect properties" {
                  let runtime = new Mock.CommandRuntime()
                  let cmdlet = new SelectPocofCommandForTest()
                  cmdlet.CommandRuntime <- runtime
                  cmdlet.InputObject <- [| PSObject.AsPSObject "a" |]
                  cmdlet.Query <- "a"
                  cmdlet.Matcher <- "Match"
                  cmdlet.Operator <- "Or"
                  cmdlet.CaseSensitive <- true
                  cmdlet.InvertQuery <- true
                  cmdlet.NonInteractive <- true
                  cmdlet.SuppressProperties <- true
                  cmdlet.Unique <- true
                  cmdlet.Prompt <- ">"
                  cmdlet.Layout <- "TopDown"

                  cmdlet.Keymaps <-
                      let k = new Hashtable()
                      k.Add("Escape", "Finish")
                      k

                  cmdlet.WordDelimiters <- "_"
              // NOTE: do nothing
              }

              ]
