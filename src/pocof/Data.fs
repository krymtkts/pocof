namespace pocof

open System
open System.Management.Automation
open System.Collections

module PocofData =
    type Entry =
        | Obj of PSObject
        | Dict of DictionaryEntry

    let unwrap (entries: Entry list) =
        entries
        |> List.map (fun o ->
            match o with
            | Dict (dct) -> dct :> obj
            | Obj (o) -> o)

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

    type PropertySearch =
        | NonSearch
        | Search of string

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
          PropertySearch: PropertySearch
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
          Keymaps: Hashtable
          NotInteractive: bool }

    let private convertKeymaps (h: Hashtable) =
        if h = null then
            Map []
        else
            h
            |> Seq.cast<DictionaryEntry>
            |> Seq.map (fun e ->
                let k = e.Key.ToString()
                let v = Action.ofString (e.Value.ToString())
                (k, v))
            |> Map.ofSeq

    let initConfig (p: IncomingParameters) =
        { Prompt = p.Prompt
          Layout = Layout.ofString p.Layout
          Keymaps = convertKeymaps p.Keymaps
          NotInteractive = p.NotInteractive },
        { Query = p.Query
          QueryState =
            { Matcher = Matcher.ofString p.Matcher
              Operator = Operator.ofString p.Operator
              CaseSensitive = p.CaseSensitive
              Invert = p.InvertQuery }
          PropertySearch = NonSearch
          Notification = "" },
        { X = p.Query.Length; Y = 0 }

    let private getCurrentProperty (query: string) (x: int) =
        let p = query.[..x].Split [| ' ' |] |> Seq.last

        if p.StartsWith ":" then
            Search p.[1..]
        else
            NonSearch

    let private addQuery (state: InternalState) (pos: Position) (c: char) =
        let query = state.Query.Insert(pos.X, c.ToString())

        { state with
            Query = query
            PropertySearch = getCurrentProperty query pos.X },
        { pos with X = pos.X + 1 }

    let private moveBackward (state: InternalState) (pos: Position) =
        let p =
            if pos.X > 0 then
                { pos with X = pos.X - 1 }
            else
                pos

        { state with PropertySearch = getCurrentProperty state.Query p.X }, p

    let private moveForward (state: InternalState) (pos: Position) =
        let p =
            if pos.X < state.Query.Length then
                { pos with X = pos.X + 1 }
            else
                pos

        { state with PropertySearch = getCurrentProperty state.Query p.X }, p


    let private moveHead (state: InternalState) (pos: Position) =
        { state with PropertySearch = NonSearch }, { pos with X = 0 }

    let private moveTail (state: InternalState) (pos: Position) =
        { state with PropertySearch = getCurrentProperty state.Query state.Query.Length },
        { pos with X = state.Query.Length }

    let private removeBackwardChar (state: InternalState) (pos: Position) =
        let p =
            if pos.X > 0 then
                { pos with X = pos.X - 1 }
            else
                pos

        let q =
            if state.Query.Length > p.X then
                state.Query.Remove(p.X, 1)
            else
                state.Query

        { state with
            Query = q
            PropertySearch = getCurrentProperty q p.X },
        p

    let private removeForwardChar (state: InternalState) (pos: Position) =
        let q =
            if state.Query.Length > pos.X then
                state.Query.Remove(pos.X, 1)
            else
                state.Query

        { state with
            Query = q
            PropertySearch = getCurrentProperty q pos.X },
        pos

    let private removeQueryHead (state: InternalState) (pos: Position) =
        let q = state.Query.[pos.X ..]

        { state with
            Query = q
            PropertySearch = getCurrentProperty q 0 },
        { pos with X = 0 }

    let private removeQueryTail (state: InternalState) (pos: Position) =
        let q = state.Query.[.. pos.X - 1]

        { state with
            Query = q
            PropertySearch = getCurrentProperty q pos.X },
        pos

    let private switchFilter (state: InternalState) =
        { state with
            QueryState =
                { state.QueryState with
                    Matcher =
                        match state.QueryState.Matcher with
                        | EQ -> LIKE
                        | LIKE -> MATCH
                        | MATCH -> EQ } }

    let private switchOperator (state: InternalState) =
        { state with
            QueryState =
                { state.QueryState with
                    Operator =
                        match state.QueryState.Operator with
                        | OR -> AND
                        | AND -> NONE
                        | NONE -> OR } }

    let private switchCaseSensitive (state: InternalState) =
        { state with QueryState = { state.QueryState with CaseSensitive = not state.QueryState.CaseSensitive } }

    let private switchInvertFilter (state: InternalState) =
        { state with QueryState = { state.QueryState with Invert = not state.QueryState.Invert } }

    let invokeAction (action: Action) (state: InternalState) (pos: Position) =
        match action with
        | AddChar c -> addQuery state pos c
        | BackwardChar -> moveBackward state pos
        | ForwardChar -> moveForward state pos
        | BeginningOfLine -> moveHead state pos
        | EndOfLine -> moveTail state pos
        | DeleteBackwardChar -> removeBackwardChar state pos
        | DeleteForwardChar -> removeForwardChar state pos
        | KillBeginningOfLine -> removeQueryHead state pos
        | KillEndOfLine -> removeQueryTail state pos
        | RotateMatcher -> switchFilter state, pos
        | RotateOperator -> switchOperator state, pos
        | ToggleCaseSensitive -> switchCaseSensitive state, pos
        | ToggleInvertFilter -> switchInvertFilter state, pos
        | SelectUp -> state, pos // TODO: implement it.
        | SelectDown -> state, pos // TODO: implement it.
        | ToggleSelectionAndSelectNext -> state, pos // TODO: ???
        | ScrollPageUp -> state, pos // TODO: implement it.
        | ScrollPageDown -> state, pos // TODO: implement it.
        | TabExpansion -> state, pos // TODO: implement it.
        | x ->
            failwithf "unrecognized Action. value='%s'"
            <| x.GetType().Name
