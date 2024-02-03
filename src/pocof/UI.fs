namespace pocof

module PocofScreen =
    open System
    open System.Management.Automation.Host

    type IRawUI =
        inherit IDisposable
        abstract member GetCursorPosition: unit -> int * int
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

        do Console.TreatControlCAsInput <- true

        interface IRawUI with
            member __.GetCursorPosition() =
                rui.CursorPosition.X, rui.CursorPosition.Y

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

    let private note = "note>"

    type WriteScreen = PocofData.InternalState -> PocofData.Entry list -> Result<string list, string> -> unit

    type Buff(r, i, layout) =
        let rui: IRawUI = r
        let invoke: obj list -> string seq = i

        let layout: PocofData.Layout = layout

        let height =
            match layout with
            | PocofData.TopDownHalf
            | PocofData.BottomUpHalf -> rui.GetWindowHeight() / 2 - 1
            | _ -> rui.GetWindowHeight() - 1

        let pos =
            let y =
                match layout with
                | PocofData.TopDownHalf
                | PocofData.BottomUpHalf -> rui.GetCursorPosition() |> snd
                | _ -> 0

            match (y + height) - rui.GetWindowHeight() with
            | over when over > 0 -> 0, y - over - 1
            | _ -> 0, y

        do
            // NOTE: add lines to the end of the screen for scrolling using the PSReadLine method.
            rui.GetCursorPosition() ||> rui.Write
            <| String.replicate height "\n"

            let pos =
                match layout with
                | PocofData.BottomUpHalf -> pos |> fst, pos |> snd |> (+) height
                | _ -> pos

            pos ||> rui.SetCursorPosition

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
            member __.Dispose() =
                (rui :> IDisposable).Dispose()
                // TODO: not work well when the screen height is resized.
                // clear contests.
                pos ||> rui.SetCursorPosition

                let height =
                    match layout with
                    | PocofData.TopDownHalf
                    | PocofData.BottomUpHalf -> rui.GetWindowHeight() / 2
                    | _ -> rui.GetWindowHeight()

                String.replicate (rui.GetWindowWidth()) " "
                |> List.replicate height
                |> String.concat "\n"
                |> Console.Write

                pos ||> rui.SetCursorPosition

        member private __.writeScreenLine (height: int) (line: string) =
            match (rui.GetWindowWidth()
                   - __.GetLengthInBufferCells line)
                with
            | x when x > 0 -> line + String.replicate x " "
            | _ -> line
            |> rui.Write 0 height

        member private __.calculatePositions layout =
            match layout with
            | PocofData.TopDown ->
                let basePosition = 0
                basePosition, basePosition + 1, (+) (basePosition + 2), rui.GetWindowHeight() - 3
            | PocofData.TopDownHalf ->
                let basePosition = rui.GetCursorPosition() |> snd
                basePosition, basePosition + 1, (+) (basePosition + 2), rui.GetWindowHeight() / 2 - 3
            | PocofData.BottomUp ->
                let basePosition = rui.GetWindowHeight() - 1
                basePosition, basePosition - 1, (-) (basePosition - 2), rui.GetWindowHeight() - 3
            | PocofData.BottomUpHalf ->
                let basePosition = rui.GetCursorPosition() |> snd

                basePosition, basePosition - 1, (-) (basePosition - 2), rui.GetWindowHeight() / 2 - 3

        member __.writeScreen
            (layout: PocofData.Layout)
            (state: PocofData.InternalState)
            (entries: PocofData.Entry list)
            (props: Result<string list, string>)
            =
            let basePosition, firstLine, toHeight, height = __.calculatePositions layout
            let topLine = info state
            topLine |> __.writeScreenLine basePosition

#if DEBUG
            Logger.logFile [ $"basePosition {basePosition}, firstLine {firstLine}, toHeight {toHeight}, height {height}" ]
#endif

            __.writeScreenLine firstLine
            <| match state.Notification with
               | "" ->
                   match props with
                   | Ok (p) -> (String.concat " " p).[.. (rui.GetWindowWidth()) - 1]
                   | Error (e) -> note + e
               | _ -> note + state.Notification

            let out =
                match List.length entries < height with
                | true -> entries
                | _ -> List.take height entries
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

            seq { 0..height }
            |> Seq.iter (fun i ->
                __.writeScreenLine
                <| toHeight i
                <| match List.tryItem i out with
                   | Some s -> s
                   | None -> String.Empty)

            rui.SetCursorPosition
            <| rui.GetLengthInBufferCells topLine.[.. (PocofData.InternalState.getX state) - 1]
            <| basePosition

        member __.getConsoleWidth = rui.GetWindowWidth

        member __.getKey() =
            Async.FromContinuations(fun (cont, _, _) -> read [] |> cont)
            |> Async.RunSynchronously

        member __.GetLengthInBufferCells = rui.GetLengthInBufferCells

    let init (rui: unit -> IRawUI) (invoke: obj list -> string seq) (layout: PocofData.Layout) =
        new Buff(rui (), invoke, layout)
