namespace pocof

open System

module PocofData =
    type Action =
        | None
        | Cancel
        | Finish

        // move.
        | BackwardChar
        | ForwardChar
        | BeginningOfLine
        | EndOfLine

        // edit query.
        | AddChar of char
        | DeleteBackwardChar
        | DeleteForwardChar

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
            | "DeleteBackwardChar" -> DeleteBackwardChar
            | "DeleteForwardChar" -> DeleteForwardChar
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
            | _ -> failwithf "this is a unmodifiable Action. action='%s'" s


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

    let addQuery (s: InternalState) (x: int) (c: char) =
        { s with Query = s.Query.Insert(x, c.ToString()) }

    let moveLeft (s: Position) =
        if s.X > 0 then
            { s with X = s.X - 1 }
        else
            s

    let moveRight (s: Position) (ql: int) =
        if s.X < ql then
            { s with X = s.X + 1 }
        else
            s

    let moveHead (s: Position) = { s with X = 0 }
    let moveTail (s: Position) (ql: int) = { s with X = ql }

    let removeQuery (s: InternalState) (x: int) =
        { s with
            Query =
                if s.Query.Length > x then
                    s.Query.Remove(x)
                else
                    s.Query }

    let removeQueryHead (s: InternalState) (x: int) = { s with Query = s.Query.Substring(x) }

    let removeQueryTail (s: InternalState) (x: int) =
        { s with Query = s.Query.Substring(0, x) }

    let switchFilter (s: InternalState) =
        { s with
            Filter =
                match s.Filter with
                | EQ -> LIKE
                | LIKE -> MATCH
                | MATCH -> EQ }

    let switchCaseSensitive (s: InternalState) =
        { s with CaseSensitive = not s.CaseSensitive }

    let switchInvertFilter (s: InternalState) =
        { s with InvertFilter = not s.InvertFilter }

    let invokeAction (a: Action) (s: InternalState) (p: Position) =
        match a with
        | AddChar c -> (addQuery s p.X c, p)
        | BackwardChar -> (s, moveLeft p)
        | ForwardChar -> (s, moveRight p s.Query.Length)
        | BeginningOfLine -> (s, moveHead p)
        | EndOfLine -> (s, moveTail p s.Query.Length)
        | DeleteBackwardChar -> (removeQuery s p.X, p)
        | DeleteForwardChar -> (removeQuery s p.X, moveLeft p)
        | KillBeginningOfLine -> (removeQueryHead s p.X, moveHead p)
        | KillEndOfLine -> (removeQueryTail s p.X, p)
        | RotateMatcher -> (switchFilter s, p)
        | ToggleCaseSensitive -> (switchCaseSensitive s, p)
        | ToggleInvertFilter -> (switchInvertFilter s, p)
        | SelectUp -> (s, p)
        | SelectDown -> (s, p)
        | ToggleSelectionAndSelectNext -> (s, p)
        | ScrollPageUp -> (s, p)
        | ScrollPageDown -> (s, p)
        | TabExpansion -> (s, p)
        | x ->
            failwithf "unrecognized Action. value='%s'"
            <| x.GetType().Name

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
            PocofData.AddChar k.KeyChar
