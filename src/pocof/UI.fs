namespace pocof

module PocofScreen =
    open System
    open System.Management.Automation.Host

    type IRawUI =
        inherit IDisposable
        abstract member SetCursorPosition: int -> int -> unit
        abstract member GetLengthInBufferCells: string -> int
        abstract member GetWindowWidth: unit -> int
        abstract member GetWindowHeight: unit -> int
        abstract member Write: int -> int -> string -> unit
        abstract member ReadKey: bool -> ConsoleKeyInfo
        abstract member KeyAvailable: unit -> bool

    type RawUI(rui) =
        let rui: PSHostRawUserInterface = rui

        let ctrlCAsInput: bool = Console.TreatControlCAsInput

        do
            Console.TreatControlCAsInput <- true

            // NOTE: add lines to the end of the screen for scrolling using the PSReadLine method.
            String.replicate (rui.WindowSize.Height - 1) "\n"
            |> Console.Write

        interface IRawUI with
            member __.SetCursorPosition (x: int) (y: int) = rui.CursorPosition <- Coordinates(x, y)

            member __.GetLengthInBufferCells(prompt: string) = rui.LengthInBufferCells(prompt)

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

                // clear contests.
                (__ :> IRawUI).SetCursorPosition 0 0

                String.replicate rui.WindowSize.Width " "
                |> List.replicate (rui.WindowSize.Height - 1)
                |> String.concat "\n"
                |> Console.Write

                (__ :> IRawUI).SetCursorPosition 0 0

    let private note = "note>"

    type WriteScreen = PocofData.InternalState -> PocofData.Entry list -> Result<string list, string> -> unit

    type Buff(r, i) =
        let rui: IRawUI = r
        let invoke: obj list -> string seq = i

        [<TailCall>]
        let rec getQuery (w: int) (q: string) (l: int) =
            let cl = rui.GetLengthInBufferCells q

            match w - cl with
            | x when x = 0 || x = 1 -> q
            | x ->
                let l = l + (x + Math.Sign(x)) / 2
                let q = q.[..l]
#if DEBUG
                Logger.logFile [ $"l '{l}'" ]
#endif
                getQuery w q l

        let info (state: PocofData.InternalState) =
            let q =
                let q =
                    state.QueryState.Query.[state.QueryState.WindowBeginningCursor .. state.QueryState.WindowWidth
                                                                                      + state.QueryState.WindowBeginningCursor]

                let l =
                    match state.QueryState.WindowWidth - String.length q with
                    | l when l > 0 -> l
                    | _ -> 0

                q + String.replicate l " "

#if DEBUG
            Logger.logFile [ $"q '{q}' ql '{String.length q}' WindowBeginningCursor '{state.QueryState.WindowBeginningCursor}' WindowWidth '{state.QueryState.WindowWidth}'" ]
#endif
            let q =
                getQuery state.QueryState.WindowWidth q
                <| String.length q

            [ PocofData.InternalState.prompt state
              q
              PocofData.InternalState.queryInfo state ]
            |> String.concat ""

        [<TailCall>]
        let rec read (acc: ConsoleKeyInfo list) =
            let acc = rui.ReadKey true :: acc

            match rui.KeyAvailable() with
            | true -> read acc
            | _ -> List.rev acc

        interface IDisposable with
            member __.Dispose() = (rui :> IDisposable).Dispose()

        member private __.writeScreenLine (height: int) (line: string) =
            match (rui.GetWindowWidth()
                   - __.GetLengthInBufferCells line)
                with
            | x when x > 0 -> line + String.replicate x " "
            | _ -> line
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
            Logger.logFile [ $"out length '{Seq.length out}'" ]
#endif

            seq { 0..h }
            |> Seq.iter (fun i ->
                __.writeScreenLine
                <| toHeight i
                <| match List.tryItem i out with
                   | Some s -> s
                   | None -> String.Empty)

            rui.SetCursorPosition
            <| rui.GetLengthInBufferCells topLine.[.. (PocofData.InternalState.getX state) - 1]
            <| basePosition

        member __.writeTopDown: WriteScreen = __.writeScreen PocofData.TopDown
        member __.writeBottomUp: WriteScreen = __.writeScreen PocofData.BottomUp
        member __.getConsoleWidth = rui.GetWindowWidth

        member __.getKey() =
            Async.FromContinuations(fun (cont, _, _) -> read [] |> cont)
            |> Async.RunSynchronously

        member __.GetLengthInBufferCells = rui.GetLengthInBufferCells

    let init (rui: unit -> IRawUI) (invoke: obj list -> string seq) = new Buff(rui (), invoke)
