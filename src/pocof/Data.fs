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
          PropertySearch = NonSearch
          Notification = "" },
        { X = p.Query.Length; Y = 0 }

    let getCurrentProperty (q: string) (x: int) =
        let cur = q.[..x].Split [| ' ' |] |> Seq.last

        if cur.StartsWith ":" then
            Search cur.[1..]
        else
            NonSearch

    let addQuery (s: InternalState) (p: Position) (c: char) =
        let query = s.Query.Insert(p.X, c.ToString())

        { s with
            Query = query
            PropertySearch = getCurrentProperty query p.X },
        { p with X = p.X + 1 }

    let moveBackward (s: InternalState) (p: Position) =
        let p =
            if p.X > 0 then
                { p with X = p.X - 1 }
            else
                p

        { s with PropertySearch = getCurrentProperty s.Query p.X }, p

    let moveForward (s: InternalState) (p: Position) =
        let p =
            if p.X < s.Query.Length then
                { p with X = p.X + 1 }
            else
                p

        { s with PropertySearch = getCurrentProperty s.Query p.X }, p


    let moveHead (s: InternalState) (p: Position) =
        { s with PropertySearch = NonSearch }, { p with X = 0 }

    let moveTail (s: InternalState) (p: Position) =
        { s with PropertySearch = getCurrentProperty s.Query s.Query.Length }, { p with X = s.Query.Length }

    let removeBackwardChar (s: InternalState) (p: Position) =
        let p =
            if p.X > 0 then
                { p with X = p.X - 1 }
            else
                p

        let q =
            if s.Query.Length > p.X then
                s.Query.Remove(p.X, 1)
            else
                s.Query

        { s with
            Query = q
            PropertySearch = getCurrentProperty q p.X },
        p

    let removeForwardChar (s: InternalState) (p: Position) =
        let q =
            if s.Query.Length > p.X then
                s.Query.Remove(p.X, 1)
            else
                s.Query

        { s with
            Query = q
            PropertySearch = getCurrentProperty q p.X },
        p

    let removeQueryHead (s: InternalState) (p: Position) =
        let q = s.Query.[p.X ..]

        { s with
            Query = q
            PropertySearch = getCurrentProperty q 0 },
        { p with X = 0 }

    let removeQueryTail (s: InternalState) (p: Position) =
        let q = s.Query.[.. p.X - 1]

        { s with
            Query = q
            PropertySearch = getCurrentProperty q p.X },
        p

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
        | BackwardChar -> moveBackward s p
        | ForwardChar -> moveForward s p
        | BeginningOfLine -> moveHead s p
        | EndOfLine -> moveTail s p
        | DeleteBackwardChar -> removeBackwardChar s p
        | DeleteForwardChar -> removeForwardChar s p
        | KillBeginningOfLine -> removeQueryHead s p
        | KillEndOfLine -> removeQueryTail s p
        | RotateMatcher -> switchFilter s, p
        | RotateOperator -> switchOperator s, p
        | ToggleCaseSensitive -> switchCaseSensitive s, p
        | ToggleInvertFilter -> switchInvertFilter s, p
        | SelectUp -> s, p // TODO: implement it.
        | SelectDown -> s, p // TODO: implement it.
        | ToggleSelectionAndSelectNext -> s, p // TODO: ???
        | ScrollPageUp -> s, p // TODO: implement it.
        | ScrollPageDown -> s, p // TODO: implement it.
        | TabExpansion -> s, p // TODO: implement it.
        | x ->
            failwithf "unrecognized Action. value='%s'"
            <| x.GetType().Name
