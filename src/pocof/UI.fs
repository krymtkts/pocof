namespace pocof

open System
open System.Management.Automation // PowerShell attributes come from this namespace
open System.Management.Automation.Host // PowerShell attributes come from this namespace

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
    let anchor = ">"

    type Buff =
        val rui: PSHostRawUserInterface
        val buf: BufferCell [,]
        val prompt: string
        val plen: int

        new(r, p) =
            { rui = r
              buf = r.GetBufferContents(Rectangle(0, 0, r.WindowSize.Width, r.CursorPosition.Y))
              prompt = p
              plen = p.Length + anchor.Length }

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

        member __.writeRightInfo (filter: string) (length: int) (height: int) =
            let info = sprintf "%s [%d]" filter length
            let x = __.rui.WindowSize.Width - info.Length
            __.setCursorPosition x height
            Console.Write info

        member __.writeScreenLine (height: int) (line: string) =
            __.setCursorPosition 0 height

            line.PadRight __.rui.WindowSize.Width
            |> Console.Write // TODO: replce write-host <- not console.wrte

        member __.writeTopDown (prompt: string) (filter: string) (x: int) (entries: PSObject list) =
            __.writeScreenLine 0 <| prompt + ">"
            __.writeRightInfo filter entries.Length 0

            let h = __.rui.WindowSize.Height

            // TODO: can i use Format-Table ?
            seq { 0 .. h - 2 }
            |> Seq.iter (fun i ->
                match List.tryItem i entries with
                | Some x -> __.writeScreenLine <| i + 1 <| x.ToString()
                | None -> __.writeScreenLine <| i + 1 <| String.Empty)

            __.setCursorPosition
            <| __.getCursorPositionX filter x
            <| 0

        member __.writeBottomUp (prompt: string) (filter: string) (x: int) (entries: PSObject list) =
            // TODO: implement it from Write-BottomUp.
            __.setCursorPosition
            <| __.getCursorPositionX filter x
            <| __.rui.CursorPosition.Y


    let init (rui: PSHostRawUserInterface) (prompt: string) =
        let buf = new Buff(rui, prompt)
        Console.Clear()
        buf
