namespace pocof

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

        let buf: BufferCell [,] option =
            try
                rui.GetBufferContents(Rectangle(0, 0, rui.WindowSize.Width, rui.CursorPosition.Y))
                |> Some
            with // NOTE: when running on Linux, this exception is thrown.
            | :? NotImplementedException -> None

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

                let pos =
                    match buf with
                    | Some buf ->
                        rui.SetBufferContents(origin, buf)
                        buf.GetUpperBound 0
                    | None -> 0

                (__ :> IRawUI).SetCursorPosition 0 pos

    let private anchor = ">"
    let private note = "note>"

    type WriteScreen = PocofData.InternalState -> int -> PocofData.Entry list -> Result<string list, string> -> unit

    type Buff(r, p, i) =
        let rui: IRawUI = r
        let prompt: string = p
        let invoke: obj list -> string seq = i

        interface IDisposable with
            member __.Dispose() = (rui :> IDisposable).Dispose()

        member private __.writeQueryInfo (state: PocofData.InternalState) (length: int) (row: int) =
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
            __.writeQueryInfo state entries.Length basePosition

#if DEBUG
            Logger.logFile [ List.length entries ]
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
                        // NOTE: This split lines is implemented complicated way because of netstandard2.0.
                        s
                        |> String.replace Environment.NewLine "\n"
                        |> String.split "\n"
                        |> List.ofArray
                        |> (@) acc)
                    []

#if DEBUG
            Logger.logFile [ "out length"
                             $"{Seq.length out}" ]
#endif

            seq { 0..h }
            |> Seq.iter (fun i ->
                __.writeScreenLine
                <| toHeight i
                <| match List.tryItem i out with
                   | Some s -> s
                   | None -> String.Empty)

            rui.SetCursorPosition
            <| rui.GetCursorPositionX topLine (prompt.Length + anchor.Length + x)
            <| basePosition

        member __.writeTopDown: WriteScreen = __.writeScreen PocofData.TopDown

        member __.writeBottomUp: WriteScreen = __.writeScreen PocofData.BottomUp

    let init (rui: PSHostRawUserInterface) (prompt: string) (invoke: obj list -> string seq) =
        let r = new RawUI(rui)
        let buf = new Buff(r, prompt, invoke)
        buf
