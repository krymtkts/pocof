namespace pocof

module PocofScreen =
    open System
    open System.Management.Automation.Host

    type IRawUI =
        inherit IDisposable
        abstract member SetCursorPosition: int -> int -> unit
        abstract member GetCursorPositionX: string -> int
        abstract member GetWindowWidth: unit -> int
        abstract member GetWindowHeight: unit -> int
        abstract member Write: int -> int -> string -> unit
        abstract member ReadKey: bool -> ConsoleKeyInfo
        abstract member KeyAvailable: unit -> bool

    type RawUI(rui) =
        let rui: PSHostRawUserInterface = rui

        // TODO: replace backup/restore buffer contents with scrolling contents for Linux support.
        let buf: BufferCell [,] option =
            try
                rui.GetBufferContents(Rectangle(0, 0, rui.WindowSize.Width, rui.CursorPosition.Y))
                |> Some
            with // NOTE: when running on Linux, this exception is thrown.
            | :? NotImplementedException -> None

        let ctrlCAsInput: bool = Console.TreatControlCAsInput

        do
            Console.TreatControlCAsInput <- true
            Console.Clear()

        interface IRawUI with
            member __.SetCursorPosition (x: int) (y: int) = rui.CursorPosition <- Coordinates(x, y)

            member __.GetCursorPositionX(prompt: string) = rui.LengthInBufferCells(prompt)

            member __.GetWindowWidth() = rui.WindowSize.Width
            member __.GetWindowHeight() = rui.WindowSize.Height

            member __.Write (x: int) (y: int) (s: string) =
                (__ :> IRawUI).SetCursorPosition x y
                Console.Write s

            member __.ReadKey(intercept: bool) = Console.ReadKey intercept
            member __.KeyAvailable() = Console.KeyAvailable

        interface IDisposable with
            member __.Dispose() =
                Console.TreatControlCAsInput <- ctrlCAsInput
                Console.Clear()
                let origin = Coordinates(0, 0)

                let pos =
                    match buf with
                    | Some buf ->
                        rui.SetBufferContents(origin, buf)
                        buf.GetUpperBound 0
                    | None -> 0

                (__ :> IRawUI).SetCursorPosition 0 pos

    let private note = "note>"

    type WriteScreen = PocofData.InternalState -> PocofData.Entry list -> Result<string list, string> -> unit

    type Buff(r, i) =
        let rui: IRawUI = r
        let invoke: obj list -> string seq = i

        let info (state: PocofData.InternalState) =
            let left = PocofData.InternalState.prompt state
            let right = PocofData.InternalState.queryInfo state
            let l = PocofData.InternalState.getWindowWidth state

            let q =
                // TODO: should use RawUI.LengthInBufferCells instead of String.length for supporting full-width characters.
                match String.length state.QueryState.Query with
                | ql when ql < l -> ql
                | _ -> state.QueryState.WindowBeginningX + l
                |> fun ql -> state.QueryState.Query.[state.QueryState.WindowBeginningX .. ql - 1]
                |> String.padRight l

#if DEBUG
            Logger.logFile [ $"q '{String.length q}' WindowBeginningX '{state.QueryState.WindowBeginningX}' WindowWidth '{l}' ConsoleWidth '{state.ConsoleWidth}'" ]
#endif

            left + q + right

        [<TailCall>]
        let rec read (acc: ConsoleKeyInfo list) =
            let acc = rui.ReadKey true :: acc

            match rui.KeyAvailable() with
            | true -> read acc
            | _ -> List.rev acc

        interface IDisposable with
            member __.Dispose() = (rui :> IDisposable).Dispose()

        member private __.writeScreenLine (height: int) (line: string) =
            line.PadRight(rui.GetWindowWidth())
            |> rui.Write 0 height

        member __.writeScreen
            (layout: PocofData.Layout)
            (state: PocofData.InternalState)
            (entries: PocofData.Entry list)
            (props: Result<string list, string>)
            =
            let basePosition, firstLine, toHeight =
                match layout with
                | PocofData.TopDown -> 0, 1, (+) 2
                | PocofData.BottomUp ->
                    let basePosition = rui.GetWindowHeight() - 1
                    basePosition, basePosition - 1, (-) (basePosition - 2)

            let topLine = info state
            topLine |> __.writeScreenLine basePosition

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
            <| rui.GetCursorPositionX topLine.[.. (PocofData.InternalState.getX state) - 1]
            <| basePosition

        member __.writeTopDown: WriteScreen = __.writeScreen PocofData.TopDown
        member __.writeBottomUp: WriteScreen = __.writeScreen PocofData.BottomUp
        member __.getConsoleWidth() = rui.GetWindowWidth()

        member __.getKey() =
            Async.FromContinuations(fun (cont, _, _) -> read [] |> cont)
            |> Async.RunSynchronously

    let init (rui: PSHostRawUserInterface) (invoke: obj list -> string seq) =
        let r = new RawUI(rui)
        let buf = new Buff(r, invoke)
        buf
