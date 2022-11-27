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
        | RotateOperator
        | ToggleCaseSensitive
        | ToggleInvertFilter
        | ToggleSelectionAndSelectNext
        // move selection.
        | SelectUp
        | SelectDown
        | ScrollPageUp
        | ScrollPageDown
        // autocomplete
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
            | "RotateOperator" -> RotateOperator
            | "ToggleCaseSensitive" -> ToggleCaseSensitive
            | "ToggleInvertFilter" -> ToggleInvertFilter
            | "ToggleSelectionAndSelectNext" -> ToggleSelectionAndSelectNext
            | "SelectUp" -> SelectUp
            | "SelectDown" -> SelectDown
            | "ScrollPageUp" -> ScrollPageUp
            | "ScrollPageDown" -> ScrollPageDown
            | "TabExpansion" -> TabExpansion
            | _ -> failwithf "this is a unmodifiable Action. action='%s'" s


    type Matcher =
        | EQ
        | LIKE
        | MATCH
        static member ofString(s: string) =
            match s.ToUpper() with
            | "EQ" -> EQ
            | "LIKE" -> LIKE
            | "MATCH" -> MATCH
            | _ -> failwithf "unrecognized Matcher. value='%s'" s

    type Operator =
        | AND
        | OR
        | NONE
        static member ofString(s: string) =
            match s.ToUpper() with
            | "AND" -> AND
            | "OR" -> OR
            | "NONE" -> NONE
            | _ -> failwithf "unrecognized Operator. value='%s'" s

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
          Keymaps: Map<String, Action>
          NotInteractive: bool } // TODO: enhance this map.

    type QueryState =
        { Matcher: Matcher
          Operator: Operator
          CaseSensitive: bool
          Invert: bool }
        member __.toString =
            match __.Matcher with
            | EQ ->
                [ if __.CaseSensitive then "c" else ""
                  if __.Invert then "ne" else "eq" ]
            | _ ->
                [ if __.Invert then "not" else ""
                  if __.CaseSensitive then "c" else ""
                  __.Matcher.ToString().ToLower() ]
            |> String.concat ""
            |> (+)
            <| " " + __.Operator.ToString().ToLower()

    type InternalState =
        { Query: string
          QueryState: QueryState
          Notification: string }

    type Position = { X: int; Y: int }

    type IncomingParameters =
        { Query: string
          Matcher: string
          Operator: string
          CaseSensitive: bool
          InvertQuery: bool
          Prompt: string
          Layout: string
          Keymaps: Collections.Hashtable
          NotInteractive: bool }

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
        { Prompt = p.Prompt
          Layout = Layout.ofString p.Layout
          Keymaps = getKeyMaps p.Keymaps
          NotInteractive = p.NotInteractive },
        { Query = p.Query
          QueryState =
            { Matcher = Matcher.ofString p.Matcher
              Operator = Operator.ofString p.Operator
              CaseSensitive = p.CaseSensitive
              Invert = p.InvertQuery }
          Notification = "" },
        { X = p.Query.Length; Y = 0 }

    let addQuery (s: InternalState) (p: Position) (c: char) =
        let newS = { s with Query = s.Query.Insert(p.X, c.ToString()) }
        newS, { p with X = p.X + 1 }

    let moveBackward (p: Position) =
        if p.X > 0 then
            { p with X = p.X - 1 }
        else
            p

    let moveForward (p: Position) (ql: int) =
        if p.X < ql then
            { p with X = p.X + 1 }
        else
            p

    let moveHead (p: Position) = { p with X = 0 }
    let moveTail (p: Position) (ql: int) = { p with X = ql }

    let removeBackwardChar (s: InternalState) (p: Position) =
        let p = moveBackward p

        { s with
            Query =
                if s.Query.Length > p.X then
                    s.Query.Remove(p.X, 1)
                else
                    s.Query },
        p

    let removeForwardChar (s: InternalState) (p: Position) =
        { s with
            Query =
                if s.Query.Length > p.X then
                    s.Query.Remove(p.X, 1)
                else
                    s.Query },
        p

    let removeQueryHead (s: InternalState) (x: int) = { s with Query = s.Query.Substring(x) }

    let removeQueryTail (s: InternalState) (x: int) =
        { s with Query = s.Query.Substring(0, x) }

    let switchFilter (s: InternalState) =
        { s with
            QueryState =
                { s.QueryState with
                    Matcher =
                        match s.QueryState.Matcher with
                        | EQ -> LIKE
                        | LIKE -> MATCH
                        | MATCH -> EQ } }

    let switchOperator (s: InternalState) =
        { s with
            QueryState =
                { s.QueryState with
                    Operator =
                        match s.QueryState.Operator with
                        | OR -> AND
                        | AND -> NONE
                        | NONE -> OR } }

    let switchCaseSensitive (s: InternalState) =
        { s with QueryState = { s.QueryState with CaseSensitive = not s.QueryState.CaseSensitive } }

    let switchInvertFilter (s: InternalState) =
        { s with QueryState = { s.QueryState with Invert = not s.QueryState.Invert } }

    let invokeAction (a: Action) (s: InternalState) (p: Position) =
        match a with
        | AddChar c -> addQuery s p c
        | BackwardChar -> s, moveBackward p
        | ForwardChar -> s, moveForward p s.Query.Length
        | BeginningOfLine -> s, moveHead p
        | EndOfLine -> s, moveTail p s.Query.Length
        | DeleteBackwardChar -> removeBackwardChar s p
        | DeleteForwardChar -> removeForwardChar s p
        | KillBeginningOfLine -> removeQueryHead s p.X, moveHead p
        | KillEndOfLine -> removeQueryTail s p.X, p
        | RotateMatcher -> switchFilter s, p
        | RotateOperator -> switchOperator s, p
        | ToggleCaseSensitive -> switchCaseSensitive s, p
        | ToggleInvertFilter -> switchInvertFilter s, p
        | SelectUp -> s, p
        | SelectDown -> s, p
        | ToggleSelectionAndSelectNext -> s, p
        | ScrollPageUp -> s, p
        | ScrollPageDown -> s, p
        | TabExpansion -> s, p
        | x ->
            failwithf "unrecognized Action. value='%s'"
            <| x.GetType().Name
