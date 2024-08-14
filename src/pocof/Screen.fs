namespace Pocof

module Screen =
    open System
    open System.Management.Automation.Host
    open System.Threading

    [<Interface>]
    type IConsoleInterface =
        abstract member ReadKey: bool -> ConsoleKeyInfo
        abstract member Write: string -> unit
        abstract member TreatControlCAsInput: bool with get, set
        abstract member CursorVisible: bool with get, set
        abstract member KeyAvailable: bool with get

    [<Sealed>]
    type ConsoleInterface() =
        interface IConsoleInterface with

            member __.ReadKey(intercept: bool) = Console.ReadKey intercept
            member __.Write(s: string) = Console.Write s

            member __.TreatControlCAsInput
                with get () = Console.TreatControlCAsInput
                and set (v: bool): unit = Console.TreatControlCAsInput <- v

            member __.CursorVisible
                with get () = Console.CursorVisible
                and set (v: bool): unit = Console.CursorVisible <- v

            member __.KeyAvailable = Console.KeyAvailable

    [<Interface>]
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
        abstract member HideCursorWhileRendering: unit -> IDisposable

    [<Sealed>]
    type RawUI(rui, console) =
        let rui: PSHostRawUserInterface = rui
        let console: IConsoleInterface = console

        let ctrlCAsInput: bool = console.TreatControlCAsInput

        do console.TreatControlCAsInput <- true

        interface IRawUI with
            member __.GetCursorPosition() =
                rui.CursorPosition.X, rui.CursorPosition.Y

            member __.SetCursorPosition (x: int) (y: int) = rui.CursorPosition <- Coordinates(x, y)

            member __.GetLengthInBufferCells(prompt: string) = rui.LengthInBufferCells(prompt)

            member __.GetWindowWidth() = rui.WindowSize.Width
            member __.GetWindowHeight() = rui.WindowSize.Height

            member __.Write (x: int) (y: int) (s: string) =
                (__ :> IRawUI).SetCursorPosition x y
                console.Write s

            member __.ReadKey(intercept: bool) = console.ReadKey intercept
            member __.KeyAvailable() = console.KeyAvailable

            member __.HideCursorWhileRendering() =
                console.CursorVisible <- false

                { new IDisposable with
                    member _.Dispose() = console.CursorVisible <- true }

        interface IDisposable with
            member __.Dispose() =
                console.TreatControlCAsInput <- ctrlCAsInput

    let private note = "note>"

    type WriteScreen = Data.InternalState -> Data.Entry seq -> Result<string list, string> -> unit

    let escapeSequenceInvert = "\x1b[7m"
    let escapeSequenceResetInvert = "\x1b[27m"

    [<Sealed>]
    type Buff(r, i, layout) =
        let rui: IRawUI = r
        let invoke: obj seq -> string seq = i

        let layout: Data.Layout = layout

        do
            use _ = rui.HideCursorWhileRendering()

            let height =
                match layout with
                | Data.Layout.TopDownHalf
                | Data.Layout.BottomUpHalf -> 2
                | _ -> 1
                |> (/) (rui.GetWindowHeight())
                |> (+) -1

            let y =
                let y =
                    match layout with
                    | Data.Layout.TopDownHalf
                    | Data.Layout.BottomUpHalf -> rui.GetCursorPosition() |> snd
                    | _ -> 0

                match (y + height) - rui.GetWindowHeight() with
                | Negative -> y
                | over -> y - over - 1

            // NOTE: add lines to the end of the screen for scrolling using the PSReadLine method.
            rui.GetCursorPosition() ||> rui.Write <| String.replicate height "\n"

            let y =
                match layout with
                // NOTE: Required moving the cursor to the initial position for rendering in the case of BottomUpHalf.
                | Data.Layout.BottomUpHalf -> y + height
                | _ -> y

            (0, y) ||> rui.SetCursorPosition

        [<TailCall>]
        let rec getQuery (w: int) (q: string) (l: int) =
            let cl = rui.GetLengthInBufferCells q

            match w - cl with
            | 0
            | 1 -> q
            | x ->
                let l = l + (x + Math.Sign(x)) / 2
                let q = q.Substring(0, l)
                getQuery w q l

        let selectRange (queryState: Data.QueryState) (q: string) =
            match queryState.InputMode with
            | Data.InputMode.Input -> 0
            | Data.InputMode.Select(i) -> i
            |> function
                | 0 -> None
                | i ->
                    let s, e =
                        let c = queryState.Cursor - queryState.WindowBeginningCursor

                        match c, c - i with
                        | Ascending x -> x

                    let s = max s 0
                    let e = min e <| String.length q
                    Some(s, e)
            |> function
                | None -> q
                | Some(s, e) -> q.Insert(e, escapeSequenceResetInvert).Insert(s, escapeSequenceInvert)

        let info (state: Data.InternalState) =
            let q =
                let q =
                    state.QueryState.Query.Substring(state.QueryState.WindowBeginningCursor)
                    |> function
                        | x when String.length x > state.QueryState.WindowWidth ->
                            x.Substring(0, state.QueryState.WindowWidth)
                        | x -> x

                let l =
                    match state.QueryState.WindowWidth - String.length q with
                    | Natural l -> l
                    | _ -> 0

                let q = q + String.replicate l " "

#if DEBUG
                Logger.LogFile
                    [ $"query '{q}' query length '{String.length q}' WindowBeginningCursor '{state.QueryState.WindowBeginningCursor}' WindowWidth '{state.QueryState.WindowWidth}'" ]
#endif
                getQuery state.QueryState.WindowWidth q <| String.length q
                |> selectRange state.QueryState

            [ Data.InternalState.prompt state; q; Data.InternalState.queryInfo state ]
            |> String.concat ""

        [<TailCall>]
        let rec read (acc: ConsoleKeyInfo list) =
            rui.KeyAvailable()
            |> function
                | true ->
                    let acc = rui.ReadKey true :: acc
                    read acc
                | _ ->
                    match acc with
                    | [] ->
                        Thread.Sleep 10
                        read acc
                    | _ -> List.rev acc

        let getCursorPosition (state: Data.InternalState) =
            match state.QueryState.InputMode with
            | Data.InputMode.Input -> 0
            | Data.InputMode.Select(_) -> escapeSequenceInvert |> String.length
            |> (+) (Data.InternalState.getX state)

        interface IDisposable with
            member __.Dispose() =
                (rui :> IDisposable).Dispose()
                use _ = rui.HideCursorWhileRendering()

                let pos =
                    let y = rui.GetCursorPosition() |> snd

                    match layout with
                    | Data.Layout.BottomUp -> (0, 0)
                    | Data.Layout.BottomUpHalf -> (0, y - (rui.GetWindowHeight() / 2) + 1)
                    | _ -> (0, y)

                let height =
                    match layout with
                    | Data.Layout.TopDownHalf
                    | Data.Layout.BottomUpHalf -> pos |> snd |> (-) (rui.GetWindowHeight())
                    | _ -> rui.GetWindowHeight()

                pos ||> rui.Write
                <| (String.replicate (rui.GetWindowWidth()) " "
                    |> List.replicate height
                    |> String.concat "\n")

                pos ||> rui.SetCursorPosition

        member private __.WriteScreenLine (height: int) (line: string) =
            match (rui.GetWindowWidth() - __.GetLengthInBufferCells line) with
            | Natural x -> line + String.replicate x " "
            | _ -> line
            |> rui.Write 0 height

        member private __.CalculatePositions layout =
            match layout with
            | Data.Layout.TopDown ->
                let basePosition = 0
                basePosition, basePosition + 1, (+) (basePosition + 2), rui.GetWindowHeight() - 3
            | Data.Layout.TopDownHalf ->
                let basePosition = rui.GetCursorPosition() |> snd
                basePosition, basePosition + 1, (+) (basePosition + 2), rui.GetWindowHeight() / 2 - 3
            | Data.Layout.BottomUp ->
                let basePosition = rui.GetWindowHeight() - 1
                basePosition, basePosition - 1, (-) (basePosition - 2), rui.GetWindowHeight() - 3
            | Data.Layout.BottomUpHalf ->
                let basePosition = rui.GetCursorPosition() |> snd

                basePosition, basePosition - 1, (-) (basePosition - 2), rui.GetWindowHeight() / 2 - 3

        member __.WriteScreen
            (layout: Data.Layout)
            (state: Data.InternalState)
            (entries: Data.Entry pseq)
            (props: Result<string list, string>)
            =
            use _ = rui.HideCursorWhileRendering()

            let baseLine, firstLine, toHeight, screenHeight = __.CalculatePositions layout
            let information = info state
            information |> __.WriteScreenLine baseLine

#if DEBUG
            Logger.LogFile
                [ $"baseLine {baseLine}, firstLine {firstLine}, toHeight {toHeight}, screenHeight {screenHeight}" ]
#endif

            __.WriteScreenLine firstLine
            <| match state.Notification with
               | "" ->
                   match props with
                   | Ok(p) ->
                       let suggestion = String.concat " " p

                       let length =
                           match String.length suggestion, rui.GetWindowWidth() with
                           | Ascending(x, _) -> x

                       suggestion.Substring(0, length)
                   | Error(e) -> note + e
               | _ -> note + state.Notification

            let out =
                Seq.truncate screenHeight entries
                |> Data.unwrap
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

            seq { 0..screenHeight }
            |> Seq.iter (fun i ->
                __.WriteScreenLine
                <| toHeight i
                <| match List.tryItem i out with
                   | Some s -> s
                   | None -> String.Empty)

            rui.SetCursorPosition
            <| rui.GetLengthInBufferCells(information.Substring(0, getCursorPosition state))
            <| baseLine

        member __.GetConsoleWidth = rui.GetWindowWidth

        member __.GetKey() =
            Async.FromContinuations(fun (cont, _, _) -> read [] |> cont)
            |> Async.RunSynchronously

        member __.GetLengthInBufferCells = rui.GetLengthInBufferCells

    let init (rui: unit -> IRawUI) (invoke: obj seq -> string seq) (layout: Data.Layout) =
        new Buff(rui (), invoke, layout)
