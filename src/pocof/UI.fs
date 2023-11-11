namespace pocof

open System
open System.Management.Automation.Host

module PocofDebug =
    // for debugging.
    open System.IO

    let logFile path res =
        use sw = new StreamWriter(path, true)
        res |> List.iter (fprintfn sw "<%A>")

module PocofScreen =
    type RawUI =
        val rui: PSHostRawUserInterface
        val buf: BufferCell [,]
        val caAsInput: bool

        private new(r) =
            { rui = r
              buf = r.GetBufferContents(Rectangle(0, 0, r.WindowSize.Width, r.CursorPosition.Y))
              caAsInput = Console.TreatControlCAsInput }

        static member init(r: PSHostRawUserInterface) =
            let r = new RawUI(r)
            Console.Clear()
            r

        member __.setCursorPosition (x: int) (y: int) =
            __.rui.CursorPosition <- Coordinates(x, y)

        member __.getCursorPositionX (prompt: string) (x: int) =
            __.rui.LengthInBufferCells(prompt.Substring(0, x))

        member __.getWindowWidth() = __.rui.WindowSize.Width
        member __.getWindowHeight() = __.rui.WindowSize.Height

        interface IDisposable with
            member __.Dispose() =
                Console.TreatControlCAsInput <- __.caAsInput
                Console.Clear()
                let origin = Coordinates(0, 0)
                __.rui.SetBufferContents(origin, __.buf)
                __.setCursorPosition 0 <| __.buf.GetUpperBound 0

    let private anchor = ">"

    type Buff =
        val rui: RawUI
        val prompt: string
        val invoke: list<obj> -> seq<string>

        new(r, p, i) = { rui = r; prompt = p; invoke = i }

        interface IDisposable with
            member __.Dispose() = (__.rui :> IDisposable).Dispose()

        member private __.writeRightInfo (state: PocofData.InternalState) (length: int) (row: int) =
            let info = $"%O{state.QueryState} [%d{length}]"
            let x = (__.rui.getWindowWidth ()) - info.Length
            __.rui.setCursorPosition x row
            Console.Write info

        member private __.writeScreenLine (height: int) (line: string) =
            __.rui.setCursorPosition 0 height

            line.PadRight(__.rui.getWindowWidth ())
            |> Console.Write

        member __.writeScreen
            (layout: PocofData.Layout)
            (state: PocofData.InternalState)
            (x: int)
            (entries: PocofData.Entry list)
            (props: Result<string list, string>)
            =
            let basePosition, firstLine, toHeight =
                match layout with
                | PocofData.TopDown -> 0, 1, (+) 2
                | PocofData.BottomUp ->
                    let basePosition = __.rui.getWindowHeight () - 1
                    basePosition, basePosition - 1, (-) (basePosition - 2)

            let prompt = __.prompt + anchor + state.Query
            __.writeScreenLine basePosition prompt

            __.writeRightInfo state entries.Length basePosition

            // PocofDebug.logFile "./debug.log" [ List.length entries ]

            __.writeScreenLine firstLine
            <| match state.Notification with
               | "" ->
                   match props with
                   | Ok (p) -> (String.concat " " p).[.. (__.rui.getWindowWidth ()) - 1]
                   | Error (e) -> "note>" + e
               | _ -> "note>" + state.Notification

            let h = __.rui.getWindowHeight () - 3

            let out =
                match List.length entries < h with
                | true -> entries
                | _ -> List.take h entries
                |> PocofData.unwrap
                |> __.invoke
                |> Seq.fold
                    (fun acc s ->
                        s.Split Environment.NewLine
                        |> List.ofArray
                        |> (@) acc)
                    []

            seq { 0..h }
            |> Seq.iter (fun i ->
                __.writeScreenLine
                <| toHeight i
                <| match List.tryItem i out with
                   | Some s ->
                       // logFile "./debug.log" [ s ]
                       s
                   | None -> String.Empty)

            __.rui.setCursorPosition
            <| __.rui.getCursorPositionX prompt (__.prompt.Length + anchor.Length + x)
            <| basePosition

        member __.writeTopDown = __.writeScreen PocofData.TopDown

        member __.writeBottomUp = __.writeScreen PocofData.BottomUp

    let init (rui: PSHostRawUserInterface) (prompt: string) (invoke: list<obj> -> seq<string>) =
        let r = RawUI.init rui
        let buf = new Buff(r, prompt, invoke)
        buf
