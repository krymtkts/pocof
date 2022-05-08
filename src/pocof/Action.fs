namespace pocof

open System
open System.Management.Automation.Host // PowerShell attributes come from this namespace

module PocofData =
    type Action =
        | None
        | Cancel
        | Finish

        | BackwardChar
        | ForwardChar

        | BeginningOfLine
        | EndOfLine

        | AddChar
        | DeleteBackwardChar
        | DeleteForwardChar
        | DeleteBackwardWord

        | KillBeginningOfLine
        | KillEndOfLine
        // toggle options.
        | RotateMatcher
        | ToggleCaseSensitive
        | ToggleInvertFilter
        | ToggleSelectionAndSelectNext
        // move selection.
        | SelectUp
        | SelectDown
        | ScrollPageUp
        | ScrollPageDown
        // autocomplete?
        | TabExpansion

        static member ofString s =
            match s with
            | "None" -> None
            | "Cancel" -> Cancel
            | "Finish" -> Finish
            | "BackwardChar" -> BackwardChar
            | "ForwardChar" -> ForwardChar
            | "BeginningOfLine" -> BeginningOfLine
            | "EndOfLine" -> EndOfLine
            | "AddChar" -> AddChar
            | "DeleteBackwardChar" -> DeleteBackwardChar
            | "DeleteForwardChar" -> DeleteForwardChar
            | "DeleteBackwardWord" -> DeleteBackwardWord
            | "KillBeginningOfLine" -> KillBeginningOfLine
            | "KillEndOfLine" -> KillEndOfLine
            | "RotateMatcher" -> RotateMatcher
            | "ToggleCaseSensitive" -> ToggleCaseSensitive
            | "ToggleInvertFilter" -> ToggleInvertFilter
            | "ToggleSelectionAndSelectNext" -> ToggleSelectionAndSelectNext
            | "SelectUp" -> SelectUp
            | "SelectDown" -> SelectDown
            | "ScrollPageUp" -> ScrollPageUp
            | "ScrollPageDown" -> ScrollPageDown
            | "TabExpansion" -> TabExpansion
            | _ -> failwithf "unrecognized Action. value='%s'" s


    type Filter =
        | EQ
        | LIKE
        | MATCH
        static member ofString(s: string) =
            match s.ToUpper() with
            | "EQ" -> EQ
            | "LIKE" -> LIKE
            | "MATCH" -> MATCH
            | _ -> failwithf "unrecognized Filter. value='%s'" s

    type Layout =
        | TopDown
        | BottomUp
        static member ofString s =
            match s with
            | "TopDown" -> TopDown
            | "BottomUp" -> BottomUp
            | _ -> failwithf "unrecognized Layout. value='%s'" s

    type InternalConfig =
        { Prompt: string
          Layout: Layout
          Keymaps: Map<String, Action> } // TODO: map is bad pattern for fp.

    type InternalState =
        { Query: string
          Filter: Filter
          CaseSensitive: bool
          InvertFilter: bool }

    type Position = { X: int; Y: int }

    type IncomingParameters =
        { Query: string
          Filter: string
          CaseSensitive: bool
          InvertFilter: bool
          Prompt: string
          Layout: string
          Keymaps: Collections.Hashtable }

    let getKeyMaps (h: Collections.Hashtable) =
        if h = null then
            Map []
        else
            h
            |> Seq.cast<Collections.DictionaryEntry>
            |> Seq.map (fun e ->
                let k = e.Key.ToString()
                let v = Action.ofString (e.Value.ToString())
                (k, v))
            |> Map.ofSeq

    let initConfig (p: IncomingParameters) =
        ({ Prompt = p.Prompt
           Layout = Layout.ofString p.Layout
           Keymaps = getKeyMaps p.Keymaps },
         { Query = p.Query
           Filter = Filter.ofString p.Filter
           CaseSensitive = p.CaseSensitive
           InvertFilter = p.InvertFilter },
         { X = p.Query.Length; Y = 0 })

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
    type Buff =
        val rui: PSHostRawUserInterface
        val buf: BufferCell [,]
        new(r, b) = { rui = r; buf = b }

        interface IDisposable with
            member __.Dispose() =
                Console.Clear()
                let origin = Coordinates(0, 0)
                __.rui.SetBufferContents(origin, __.buf)
                __.setCursorPosition 0 <| __.buf.GetUpperBound 0

        member __.setCursorPosition (x: int) (y: int) =
            __.rui.CursorPosition <- Coordinates(x, y)

        member __.getCursorPositionX (prompt: string) (x) =
            __.rui.LengthInBufferCells(prompt.Substring(0, x))

        member __.writeRightInfo (filter: string) (length: int) (height: int) =
            let info = sprintf "%s [%d]" filter length
            let x = __.rui.WindowSize.Width - info.Length
            __.setCursorPosition x height
            Console.Write info // TODO: separate dependencies.

        member __.writeScreenLine (height: int) (line: string) =
            __.setCursorPosition 0 height

            line.PadRight __.rui.WindowSize.Width
            |> Console.Write // TODO: replce write-host <- not console.wrte

        member __.writeTopDown (prompt: string) (filter: string) (entries: Object list) =
            __.writeScreenLine 0 prompt
            __.writeRightInfo filter entries.Length 0

            let h = __.rui.WindowSize.Height

            seq { 0 .. h - 1 }
            |> Seq.iter (fun i ->
                match List.tryItem i entries with
                | Some x -> __.writeScreenLine i <| x.ToString()
                | None -> __.writeScreenLine i String.Empty)

            let screenX = 0 //TODO: where from?
            let x = __.getCursorPositionX prompt screenX
            __.setCursorPosition x 0

        member __.writeBottomUp (prompt: string) (filter: string) (entries: Object list) =
            // TODO: implement it from Write-BottomUp.
            let screenX = 0 //TODO: where from?
            let x = __.getCursorPositionX prompt screenX
            __.setCursorPosition x __.rui.CursorPosition.Y


    let init (rui: PSHostRawUserInterface) =
        let rect = Rectangle(0, 0, rui.WindowSize.Width, rui.CursorPosition.Y)
        let buf = new Buff(rui, rui.GetBufferContents(rect))
        Console.Clear()
        buf


module PocofAction =
    let internal keyMap =
        // TODO: cannot use map literal with Ionide.
        // TODO: change more better type structure.
        [ ("Escape", PocofData.Cancel)
          ("Control+C", PocofData.Cancel)
          ("Enter", PocofData.Finish)
          ("LeftArrow", PocofData.BackwardChar)
          ("RightArrow", PocofData.ForwardChar)
          ("Home", PocofData.BeginningOfLine)
          ("End", PocofData.EndOfLine)
          ("Backspace", PocofData.DeleteBackwardChar)
          ("Delete", PocofData.DeleteForwardChar)
          ("Alt+U", PocofData.KillBeginningOfLine)
          ("Alt+K", PocofData.KillEndOfLine)
          ("Alt+R", PocofData.RotateMatcher)
          ("Alt+C", PocofData.ToggleCaseSensitive)
          ("Alt+I", PocofData.ToggleInvertFilter)
          ("Alt+W", PocofData.DeleteBackwardWord) // ?
          ("Alt+N", PocofData.SelectUp) // ?
          ("Alt+P", PocofData.SelectDown) // ?
          ("Control+Spacebar", PocofData.ToggleSelectionAndSelectNext) // ?
          ("UpArrow", PocofData.SelectUp) // ?
          ("DownArrow", PocofData.SelectDown) // ?
          ("PageUp", PocofData.ScrollPageUp) // ?
          ("PageDown", PocofData.ScrollPageDown) // ?
          ("Tab", PocofData.TabExpansion) ] // ?
        |> Map.ofSeq

    let get (uKeyMap: Map<String, PocofData.Action>) (getKey: unit -> ConsoleKeyInfo) =
        use _ = PocofConsole.init
        let k = getKey ()

        let kstr =
            if ConsoleModifiers.IsDefined k.Modifiers then
                k.Modifiers.ToString().Replace(",", "+")
                + k.Key.ToString()
            else
                k.Key.ToString()

        if Map.containsKey kstr uKeyMap then
            uKeyMap.[kstr]
        elif Map.containsKey kstr keyMap then
            keyMap.[kstr]
        else
            PocofData.AddChar
