namespace Pocof

module Screen =
    open System
    open System.Management.Automation.Host
    open System.Threading
    open System.Text

    [<Interface>]
    type IConsoleInterface =
        abstract member ReadKey: intercept: bool -> ConsoleKeyInfo
        abstract member Write: s: string -> unit
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
        abstract member GetCursorPosition: unit -> struct (int * int)
        abstract member SetCursorPosition: x: int -> y: int -> unit
        abstract member GetLengthInBufferCells: prompt: string -> int
        abstract member GetWindowWidth: unit -> int
        abstract member GetWindowHeight: unit -> int
        abstract member Write: x: int -> y: int -> s: string -> unit
        abstract member ReadKey: intercept: bool -> ConsoleKeyInfo
        abstract member KeyAvailable: unit -> bool
        abstract member HideCursorWhileRendering: unit -> IDisposable

    [<Sealed>]
    type RawUI(rui: PSHostRawUserInterface, console: IConsoleInterface) =
        let ctrlCAsInput: bool = console.TreatControlCAsInput

        do console.TreatControlCAsInput <- true

        interface IRawUI with
            member __.GetCursorPosition() =
                struct (rui.CursorPosition.X, rui.CursorPosition.Y)

            member __.SetCursorPosition (x: int) (y: int) = rui.CursorPosition <- Coordinates(x, y)

            member __.GetLengthInBufferCells(prompt: string) = rui.LengthInBufferCells prompt

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

    type StringBuilderCache() =
        let mutable cachedKey = struct (-1, -1)
        let mutable cachedSb: StringBuilder voption = ValueNone

        member __.Get (width: int) (screenHeight: int) =
            match cachedSb with
            | ValueSome sb when struct (width, screenHeight) = cachedKey ->
                sb.Clear() |> ignore
                sb
            | _ ->
                cachedKey <- struct (width, screenHeight)
                let sb = StringBuilder((width + 1) * (screenHeight + 1))
                cachedSb <- ValueSome sb
                sb

    [<Literal>]
    let private note = "note>"

    [<Literal>]
    let escapeSequenceInvert = "\x1b[7m"

    [<Literal>]
    let escapeSequenceResetInvert = "\x1b[27m"

    let escapeSequenceLength =
        escapeSequenceInvert.Length + escapeSequenceResetInvert.Length

    [<Sealed>]
    type Buff(rui: IRawUI, invoke: obj seq -> string seq, layout: Data.Layout, prompt: string) =
        let promptLength = prompt |> String.length
        let sbCache = StringBuilderCache()

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
                    | Data.Layout.BottomUpHalf -> rui.GetCursorPosition() |> snd'
                    | _ -> 0

                match y + height - rui.GetWindowHeight() with
                | Negative -> y
                | over -> y - over - 1

            // NOTE: add lines to the end of the screen for scrolling using the PSReadLine method.
            rui.GetCursorPosition() ||*> rui.Write <| String.replicate height "\n"

            let y =
                match layout with
                // NOTE: Required moving the cursor to the initial position for rendering in the case of BottomUpHalf.
                | Data.Layout.BottomUpHalf -> y + height
                | _ -> y

            rui.SetCursorPosition 0 y

        [<TailCall>]
        let rec getQuery (w: int) (q: string) (l: int) =
            let cl = rui.GetLengthInBufferCells q

            match w - cl with
            | x when x >= 0 -> struct (q, x)
            | x ->
                let l = l + (x + Math.Sign x) / 2
                let q = q |> String.upToIndex l
                getQuery w q l

        let buildQueryString (queryState: Data.QueryState) (q: string) (remains: int) =
            let queryWithPrompt =
                let sb = StringBuilder(q.Length + remains + escapeSequenceLength)

                match queryState.InputMode with
                | Data.InputMode.Input -> 0
                | Data.InputMode.Select i -> i
                |> function
                    | 0 -> sb.Append(prompt).Append(q)
                    | i ->
                        let struct (s, e) =
                            let c = queryState.Cursor - queryState.WindowBeginningCursor

                            match c, c - i with
                            | Ascending x -> x

                        let s = max s 0
                        let e = min e q.Length

                        sb
                            .Append(prompt)
                            .Append(q, 0, s)
                            .Append(escapeSequenceInvert)
                            .Append(q, s, e - s)
                            .Append(escapeSequenceResetInvert)
                            .Append(q, e, q.Length - e)


            if remains > 0 then
                queryWithPrompt.Append(' ', remains).ToString()
            else
                queryWithPrompt.ToString()

        let getQueryString (state: Data.InternalState) =
            let q =
                let query = state.QueryState.Query
                let startIndex = state.QueryState.WindowBeginningCursor
                let windowWidth = state.QueryState.WindowWidth
                let remaining = query.Length - startIndex

                if remaining <= 0 then
                    String.Empty
                else
                    let maxLength = min remaining windowWidth
                    query.Substring(startIndex, maxLength)

#if DEBUG
            Logger.LogFile
                [ $"query '{q}' query length '{q.Length}' WindowBeginningCursor '{state.QueryState.WindowBeginningCursor}' WindowWidth '{state.QueryState.WindowWidth}'" ]
#endif
            getQuery state.QueryState.WindowWidth q q.Length
            ||*> fun adjustedQ -> buildQueryString state.QueryState adjustedQ

        let getInformationString
            (width: int)
            (state: Data.InternalState)
            (props: Result<string list, string>)
            (count: int)
            =
            let sb = StringBuilder(width)
            let info = Data.InternalState.queryInfo state count
            let available = width - info.Length

            match props with
            | Error e -> sb.Append(note).Append(e) |> ignore
            | Ok [] -> ()
            | Ok(head :: tail) ->
                sb.Append(head) |> ignore
                let mutable remaining = available - sb.Length

                let rec appendItems (items: string list) =
                    match items with
                    | [] -> ()
                    | item :: rest when remaining > 1 ->
                        sb.Append(' ') |> ignore
                        remaining <- remaining - 1

                        if item.Length <= remaining then
                            sb.Append(item) |> ignore
                            remaining <- remaining - item.Length
                            appendItems rest
                        else
                            // Append only what fits and stop
                            sb.Append(item, 0, remaining) |> ignore
                    | _ -> ()

                appendItems tail

            let messageLength = sb.Length

            match available - messageLength with
            | Natural padding -> sb.Append(' ', padding) |> ignore
            | _ -> sb.Length <- available

            sb.Append(info).ToString()

        let readKey () : ConsoleKeyInfo seq =
            let acc = ResizeArray<ConsoleKeyInfo>()
            let mutable readingKey = true

            while readingKey do
                if rui.KeyAvailable() then acc.Add(rui.ReadKey true)
                else if acc.Count = 0 then Thread.Sleep 10
                else readingKey <- false

            acc

        let getCursorPosition (state: Data.InternalState) =
            match state.QueryState.InputMode with
            | Data.InputMode.Input -> 0
            | Data.InputMode.Select(_) -> escapeSequenceInvert.Length
            |> (+) (Data.InternalState.getX promptLength state)

        let calculatePositions: unit -> struct (int * int * int * int) =
            let calcTopDown () =
                let height = rui.GetWindowHeight()
                let basePosition = 0
                struct (basePosition, basePosition + 1, basePosition + 2, height - 3)

            let calcTopDownHalf () =
                let height = rui.GetWindowHeight()
                let basePosition = rui.GetCursorPosition() |> snd'
                struct (basePosition, basePosition + 1, basePosition + 2, height / 2 - 3)

            let calcBottomUp () =
                let height = rui.GetWindowHeight()
                let basePosition = height - 1
                struct (basePosition, basePosition - 1, 0, height - 3)

            let calcBottomUpHalf () =
                let basePosition = rui.GetCursorPosition() |> snd'
                let height = rui.GetWindowHeight() / 2
                struct (basePosition, basePosition - 1, basePosition - height + 1, height - 3)

            match layout with
            | Data.Layout.TopDown -> calcTopDown
            | Data.Layout.TopDownHalf -> calcTopDownHalf
            | Data.Layout.BottomUp -> calcBottomUp
            | Data.Layout.BottomUpHalf -> calcBottomUpHalf

        let sortForLayout =
            match layout with
            | Data.Layout.TopDown
            | Data.Layout.TopDownHalf -> id
            | Data.Layout.BottomUp
            | Data.Layout.BottomUpHalf -> Seq.rev

        interface IDisposable with
            member __.Dispose() =
                (rui :> IDisposable).Dispose()
                use _ = rui.HideCursorWhileRendering()

                let pos =
                    let y = rui.GetCursorPosition() |> snd'

                    match layout with
                    | Data.Layout.BottomUp -> 0, 0
                    | Data.Layout.BottomUpHalf -> 0, y - rui.GetWindowHeight() / 2 + 1
                    | _ -> 0, y

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

        member private __.GenerateScreenLine (width: int) (line: string) =
            match width - __.GetLengthInBufferCells line with
            | Natural x -> line + String(' ', x)
            | _ -> line

        member private __.AppendScreenLine (sb: StringBuilder) (width: int) (line: string) =
            match width - __.GetLengthInBufferCells line with
            | Natural x -> sb.Append(line).Append(' ', x) |> ignore
            | _ -> sb.Append(line) |> ignore

        member private __.WriteScreenLine (width: int) (height: int) (line: string) =
            __.GenerateScreenLine width line |> rui.Write 0 height

        member __.WriteScreen
            (state: Data.InternalState)
            (entries: Data.Entry pseq)
            (props: Result<string list, string>)
            =
            use _ = rui.HideCursorWhileRendering()
            let width = rui.GetWindowWidth()

            let struct (baseLine, firstLine, toHeight, screenHeight) = calculatePositions ()
            let queryString = getQueryString state
            queryString |> __.WriteScreenLine width baseLine

#if DEBUG
            Logger.LogFile
                [ $"baseLine {baseLine}, firstLine {firstLine}, toHeight {toHeight}, screenHeight {screenHeight}" ]
#endif

            getInformationString width state props (PSeq.length entries)
            |> __.WriteScreenLine width firstLine

            let out =
                Seq.truncate screenHeight entries
                |> Data.unwrap
                |> invoke
                // NOTE: This split lines is implemented complicated way because of netstandard2.0.
                |> Seq.collect (String.split2 [| "\r\n"; "\n" |])

            let sb = sbCache.Get width screenHeight

            Seq.append out (Seq.initInfinite (fun _ -> String.Empty))
            |> Seq.truncate (screenHeight + 1)
            |> sortForLayout
            |> Seq.iteri (fun i s ->
                if i > 0 then
                    sb.Append('\n') |> ignore

                __.AppendScreenLine sb width s)

            rui.Write 0 toHeight <| sb.ToString()

            rui.SetCursorPosition
            <| rui.GetLengthInBufferCells(queryString |> String.upToIndex (getCursorPosition state))
            <| baseLine

        member __.GetConsoleWidth = rui.GetWindowWidth

        member __.GetKey() = readKey ()

        member __.GetLengthInBufferCells = rui.GetLengthInBufferCells

    let init (rui: unit -> IRawUI) (invoke: obj seq -> string seq) (layout: Data.Layout) (prompt: string) =
        new Buff(rui (), invoke, layout, prompt)
