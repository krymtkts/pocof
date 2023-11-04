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
    let private anchor = ">"

    type Buff =
        val rui: PSHostRawUserInterface
        val prompt: string
        val invoke: list<obj> -> seq<string>

        val buf: BufferCell [,]
        val promptLength: int
        val caAsInput: bool

        new(r, p, i, b) =
            { rui = r
              buf = r.GetBufferContents(Rectangle(0, 0, r.WindowSize.Width, r.CursorPosition.Y))
              prompt = p
              promptLength = p.Length + anchor.Length
              invoke = i
              caAsInput = b }

        interface IDisposable with
            member __.Dispose() =
                Console.TreatControlCAsInput <- __.caAsInput
                Console.Clear()
                let origin = Coordinates(0, 0)
                __.rui.SetBufferContents(origin, __.buf)
                __.setCursorPosition 0 <| __.buf.GetUpperBound 0

        member private __.setCursorPosition (x: int) (y: int) =
            __.rui.CursorPosition <- Coordinates(x, y)

        member private __.getCursorPositionX (filter: string) (x: int) =
            __.rui.LengthInBufferCells(
                (__.prompt + anchor + filter)
                    .Substring(0, __.promptLength + x)
            )

        member private __.writeRightInfo (state: PocofData.InternalState) (length: int) (row: int) =
            let info = sprintf "%O [%d]" <| state.QueryState <| length

            let x = __.rui.WindowSize.Width - info.Length
            __.setCursorPosition x row
            Console.Write info

        member private __.writeScreenLine (height: int) (line: string) =
            __.setCursorPosition 0 height

            line.PadRight __.rui.WindowSize.Width
            |> Console.Write

        member __.writeTopDown
            (state: PocofData.InternalState)
            (x: int)
            (entries: PocofData.Entry list)
            (props: Result<string list, string>)
            =
            let basePosition = 0

            __.writeScreenLine basePosition
            <| __.prompt + ">" + state.Query

            __.writeRightInfo state entries.Length basePosition

            // PocofDebug.logFile "./debug.log" [ List.length entries ]

            __.writeScreenLine 1
            <| match state.Notification with
               | "" ->
                   match props with
                   | Ok (p) -> (String.concat " " p).[.. __.rui.WindowSize.Width - 1]
                   | Error (e) -> "note>" + e
               | _ -> "note>" + state.Notification

            let h = __.rui.WindowSize.Height - 3

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
                match List.tryItem i out with
                | Some s -> __.writeScreenLine <| i + 2 <| s
                // logFile "./debug.log" [ s ]
                | None -> __.writeScreenLine <| i + 2 <| String.Empty)

            __.setCursorPosition
            <| __.getCursorPositionX state.Query x
            <| basePosition

        member __.writeBottomUp
            (state: PocofData.InternalState)
            (x: int)
            (entries: PocofData.Entry list)
            (props: Result<string list, string>)
            =
            let basePosition = __.rui.WindowSize.Height - 1

            __.setCursorPosition
            <| __.getCursorPositionX state.Query x
            <| basePosition

            __.writeScreenLine basePosition
            <| __.prompt + ">" + state.Query

            __.writeRightInfo state entries.Length basePosition

            // PocofDebug.logFile "./debug.log" [ List.length entries ]

            __.writeScreenLine (basePosition - 1)
            <| match state.Notification with
               | "" ->
                   match props with
                   | Ok (p) -> (String.concat " " p).[.. __.rui.WindowSize.Width - 1]
                   | Error (e) -> "note>" + e
               | _ -> "note>" + state.Notification

            let h = __.rui.WindowSize.Height - 3

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
                match List.tryItem i out with
                | Some s -> __.writeScreenLine <| basePosition - i - 2 <| s
                // logFile "./debug.log" [ s ]
                | None ->
                    __.writeScreenLine
                    <| basePosition - i - 2
                    <| String.Empty)

            __.setCursorPosition
            <| __.getCursorPositionX state.Query x
            <| basePosition

    let init (rui: PSHostRawUserInterface) (prompt: string) (invoke: list<obj> -> seq<string>) =
        let buf = new Buff(rui, prompt, invoke, Console.TreatControlCAsInput)
        Console.Clear()
        Console.TreatControlCAsInput <- true
        buf
