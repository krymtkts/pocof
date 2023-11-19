namespace pocof

// for debugging.
module PocofDebug =
    open System
    open System.IO
    open System.Runtime.CompilerServices
    open System.Runtime.InteropServices

    let lockObj = new obj ()

    let logPath = "./debug.log"

    [<AbstractClass; Sealed>]
    type Logger =
        static member logFile
            (
                res,
                [<Optional; DefaultParameterValue(""); CallerMemberName>] caller: string,
                [<CallerFilePath; Optional; DefaultParameterValue("")>] path: string,
                [<CallerLineNumber; Optional; DefaultParameterValue(0)>] line: int
            ) =

            // NOTE: lock to avoid another process error when dotnet test.
            lock lockObj (fun () ->
                use sw = new StreamWriter(logPath, true)

                res
                |> List.iter (fun r ->
                    fprintfn
                        sw
                        "[%s] %s at %d %s <%A>"
                        (DateTimeOffset.Now.ToString("yyyy-MM-dd'T'HH:mm:ss.fffzzz"))
                        path
                        line
                        caller
                        r))


module PocofScreen =
    open System
    open System.Management.Automation.Host

    type IRawUI =
        inherit IDisposable
        abstract member SetCursorPosition: int -> int -> unit
        abstract member GetCursorPositionX: string -> int -> int
        abstract member GetWindowWidth: unit -> int
        abstract member GetWindowHeight: unit -> int
        abstract member Write: int -> int -> string -> unit

    type RawUI(rui) =
        let rui: PSHostRawUserInterface = rui

        let buf: BufferCell [,] =
            rui.GetBufferContents(Rectangle(0, 0, rui.WindowSize.Width, rui.CursorPosition.Y))

        let caAsInput: bool = Console.TreatControlCAsInput

        do
            Console.TreatControlCAsInput <- true
            Console.Clear()

        interface IRawUI with
            member __.SetCursorPosition (x: int) (y: int) = rui.CursorPosition <- Coordinates(x, y)

            member __.GetCursorPositionX (prompt: string) (x: int) =
                rui.LengthInBufferCells(prompt.Substring(0, x))

            member __.GetWindowWidth() = rui.WindowSize.Width
            member __.GetWindowHeight() = rui.WindowSize.Height

            member __.Write (x: int) (y: int) (s: string) =
                (__ :> IRawUI).SetCursorPosition x y
                Console.Write s

        interface IDisposable with
            member __.Dispose() =
                Console.TreatControlCAsInput <- caAsInput
                Console.Clear()
                let origin = Coordinates(0, 0)
                rui.SetBufferContents(origin, buf)

                (__ :> IRawUI).SetCursorPosition 0
                <| buf.GetUpperBound 0

    let private anchor = ">"
    let private note = "note>"

    type Buff(r, p, i) =
        let rui: IRawUI = r
        let prompt: string = p
        let invoke: obj list -> string seq = i

        interface IDisposable with
            member __.Dispose() = (rui :> IDisposable).Dispose()

        member private __.writeRightInfo (state: PocofData.InternalState) (length: int) (row: int) =
            let info = $"%O{state.QueryState} [%d{length}]"
            let x = (rui.GetWindowWidth()) - info.Length
            rui.Write x row info

        member private __.writeScreenLine (height: int) (line: string) =
            line.PadRight(rui.GetWindowWidth())
            |> rui.Write 0 height

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
                    let basePosition = rui.GetWindowHeight() - 1
                    basePosition, basePosition - 1, (-) (basePosition - 2)

            let topLine = prompt + anchor + state.Query
            __.writeScreenLine basePosition topLine
            __.writeRightInfo state entries.Length basePosition

#if DEBUG
            PocofDebug.Logger.logFile [ List.length entries ]
#endif

            __.writeScreenLine firstLine
            <| match state.Notification with
               | "" ->
                   match props with
                   | Ok (p) -> (String.concat " " p).[.. (rui.GetWindowWidth()) - 1]
                   | Error (e) -> note + e
               | _ -> note + state.Notification

            let h = rui.GetWindowHeight() - 3

            let out =
                match List.length entries < h with
                | true -> entries
                | _ -> List.take h entries
                |> PocofData.unwrap
                |> invoke
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
#if DEBUG
                       PocofDebug.Logger.logFile [ s ]
#endif
                       s
                   | None -> String.Empty)

            rui.SetCursorPosition
            <| rui.GetCursorPositionX topLine (prompt.Length + anchor.Length + x)
            <| basePosition

        member __.writeTopDown = __.writeScreen PocofData.TopDown

        member __.writeBottomUp = __.writeScreen PocofData.BottomUp

    let init (rui: PSHostRawUserInterface) (prompt: string) (invoke: obj list -> string seq) =
        let r = new RawUI(rui)
        let buf = new Buff(r, prompt, invoke)
        buf
