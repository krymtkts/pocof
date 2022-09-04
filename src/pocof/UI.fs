namespace pocof

open System
open System.Management.Automation
open System.Management.Automation.Host

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

module PocofConsole =
    type KeyState =
        val caAsInput: bool
        new(s) = { caAsInput = s }

        interface IDisposable with
            member __.Dispose() =
                Console.TreatControlCAsInput <- __.caAsInput

    let init =
        let state = new KeyState(Console.TreatControlCAsInput)
        Console.TreatControlCAsInput <- true
        state


module PocofScreen =
    open System.IO
    let anchor = ">"

    // for debugging.
    let logFile path res =
        use sw = new StreamWriter(path, true)
        res |> List.iter (fprintfn sw "<%A>")

    type Buff =
        val rui: PSHostRawUserInterface
        val buf: BufferCell [,]
        val prompt: string
        val plen: int
        val invoke: list<PSObject> -> seq<string>

        new(r, p, i) =
            { rui = r
              buf = r.GetBufferContents(Rectangle(0, 0, r.WindowSize.Width, r.CursorPosition.Y))
              prompt = p
              plen = p.Length + anchor.Length
              invoke = i }

        interface IDisposable with
            member __.Dispose() =
                Console.Clear()
                let origin = Coordinates(0, 0)
                __.rui.SetBufferContents(origin, __.buf)
                __.setCursorPosition 0 <| __.buf.GetUpperBound 0

        member __.setCursorPosition (x: int) (y: int) =
            __.rui.CursorPosition <- Coordinates(x, y)

        member __.getCursorPositionX (filter: string) (x) =
            __.rui.LengthInBufferCells(
                (__.prompt + anchor + filter)
                    .Substring(0, __.plen + x)
            )

        member __.writeRightInfo (state: PocofData.InternalState) (length: int) (row: int) =
            let info =
                sprintf "%s [%d]"
                <| state.QueryState.toString
                <| length

            let x = __.rui.WindowSize.Width - info.Length
            __.setCursorPosition x row
            Console.Write info

        member __.writeScreenLine (height: int) (line: string) =
            __.setCursorPosition 0 height

            line.PadRight __.rui.WindowSize.Width
            |> Console.Write

        member __.writeTopDown (state: PocofData.InternalState) (x: int) (entries: PSObject list) =
            __.writeScreenLine 0
            <| __.prompt + ">" + state.Query

            __.writeRightInfo state entries.Length 0

            __.writeScreenLine 1
            <| if state.Notification = String.Empty then
                   state.Notification
               else
                   "note>" + state.Notification

            let h = __.rui.WindowSize.Height - 3

            let out =
                if List.length entries < h then
                    entries
                else
                    List.take h entries
                |> __.invoke
                |> Seq.fold
                    (fun acc s ->
                        s.Split Environment.NewLine
                        |> List.ofArray
                        |> (@) acc)
                    []

            seq { 0..h }
            |> Seq.iter (fun i ->
                match List.tryItem i out with
                | Some s -> __.writeScreenLine <| i + 2 <| s
                // logFile "./debug.log" [ s ]
                | None -> __.writeScreenLine <| i + 2 <| String.Empty)

            __.setCursorPosition
            <| __.getCursorPositionX state.Query x
            <| 0

        member __.writeBottomUp (state: PocofData.InternalState) (x: int) (entries: PSObject list) =
            // TODO: implement it from Write-BottomUp.
            __.setCursorPosition
            <| __.getCursorPositionX state.Query x
            <| __.rui.CursorPosition.Y


    let init (rui: PSHostRawUserInterface) (prompt: string) (invoke: list<PSObject> -> seq<string>) =
        let buf = new Buff(rui, prompt, invoke)
        Console.Clear()
        buf
