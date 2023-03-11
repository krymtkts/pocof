namespace pocof

open System
open System.Management.Automation
open System.Collections
open Microsoft.FSharp.Reflection

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

    let private tryFromStringExcludes<'a> (excludes: Set<string>) s =
        match FSharpType.GetUnionCases typeof<'a>
              |> Seq.filter (fun u -> Set.contains u.Name excludes |> not)
              |> Seq.tryFind (fun u -> u.Name = s)
            with
        | Some u -> Ok <| (FSharpValue.MakeUnion(u, [||]) :?> 'a)
        | _ -> Error <| sprintf "Unknown case '%s'." s

    let private fromString<'a> s =
        match FSharpType.GetUnionCases typeof<'a>
              |> Seq.tryFind (fun u -> u.Name = s)
            with
        | Some u -> FSharpValue.MakeUnion(u, [||]) :?> 'a
        | _ -> failwithf "Unknown case '%s'." s

    type Action =
        | Noop
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
        | ToggleSuppressProperties
        // move selection.
        | SelectUp
        | SelectDown
        | ScrollPageUp
        | ScrollPageDown
        // autocomplete
        | TabExpansion
        static member fromString = tryFromStringExcludes<Action> <| set [ "AddChar" ]

    type Matcher =
        | EQ
        | LIKE
        | MATCH
        static member fromString = fromString<Matcher>

    type Operator =
        | AND
        | OR
        | NONE
        static member fromString = fromString<Operator>

    type Layout =
        | TopDown
        | BottomUp
        static member fromString = fromString<Layout>

    type PropertySearch =
        | NonSearch
        | Search of string

    type KeyPattern = { Modifier: int; Key: ConsoleKey }

    type InternalConfig =
        { Prompt: string
          Layout: Layout
          Keymaps: Map<KeyPattern, Action>
          NotInteractive: bool }

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
          Notification: string
          SuppressProperties: bool }

    type Position = { X: int; Y: int }

    type IncomingParameters =
        { Query: string
          Matcher: string
          Operator: string
          CaseSensitive: bool
          InvertQuery: bool
          NotInteractive: bool
          SuppressProperties: bool
          Prompt: string
          Layout: string
          Keymaps: Map<KeyPattern, Action> }

    let initConfig (p: IncomingParameters) =
        { Prompt = p.Prompt
          Layout = Layout.fromString p.Layout
          Keymaps = p.Keymaps
          NotInteractive = p.NotInteractive },
        { Query = p.Query
          QueryState =
            { Matcher = Matcher.fromString <| p.Matcher.ToUpper()
              Operator = Operator.fromString <| p.Operator.ToUpper()
              CaseSensitive = p.CaseSensitive
              Invert = p.InvertQuery }
          PropertySearch = NonSearch
          Notification = ""
          SuppressProperties = p.SuppressProperties },
        { X = p.Query.Length; Y = 0 }

    let private getCurrentProperty (query: string) (x: int) =
        let p = query.[..x].Split [| ' ' |] |> Seq.last

        if p.StartsWith ":" then
            Search <| p.[1..]
        else
            NonSearch

    let private addQuery (state: InternalState) (pos: Position) (c: char) =
        let query = state.Query.Insert(pos.X, c.ToString())
        let p = { pos with X = pos.X + 1 }

        { state with
            Query = query
            PropertySearch = getCurrentProperty query p.X },
        p

    let private moveBackward (state: InternalState) (pos: Position) =
        let p =
            if pos.X > 0 then
                { pos with X = pos.X - 1 }
            else
                pos

        { state with PropertySearch = getCurrentProperty state.Query <| p.X - 1 }, p

    let private moveForward (state: InternalState) (pos: Position) =
        let p =
            if pos.X < state.Query.Length then
                { pos with X = pos.X + 1 }
            else
                pos

        { state with PropertySearch = getCurrentProperty state.Query <| p.X - 1 }, p


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

    let private switchSuppressProperties (state: InternalState) =
        { state with SuppressProperties = not state.SuppressProperties }

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
        | ToggleSuppressProperties -> switchSuppressProperties state, pos
        | SelectUp -> state, pos // TODO: implement it.
        | SelectDown -> state, pos // TODO: implement it.
        | ScrollPageUp -> state, pos // TODO: implement it.
        | ScrollPageDown -> state, pos // TODO: implement it.
        | TabExpansion -> state, pos // TODO: implement it.
        | x ->
            failwithf "unrecognized Action. value='%s'"
            <| x.GetType().Name
