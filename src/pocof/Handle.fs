namespace Pocof

open System

open Data
open Query

module Handle =
    type QueryContext = Query.QueryContext

    let private addQuery (state: InternalState) (pos: Position) (context: QueryContext) (s: string) =
        let qs = QueryState.addQuery state.QueryState s

        let state =
            state
            |> InternalState.updateQueryState qs
            |> InternalState.refresh
            |> InternalState.prepareNotification

        state, pos, context |> QueryContext.prepareQuery state

    let private moveCursor
        (cursor: int)
        (limit: int)
        (mode: InputMode)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =
        let qs =
            QueryState.moveCursor state.QueryState cursor |> QueryState.setInputMode mode

        state
        |> InternalState.refreshIfTrue (state.QueryState.Cursor <> limit)
        |> InternalState.updateQueryState qs,
        pos,
        context

    let private moveBackwardWith = moveCursor -1 0
    let private moveBackward = moveBackwardWith InputMode.Input

    let private moveForwardWith (mode: InputMode) (state: InternalState) =
        moveCursor 1 <| String.length state.QueryState.Query <| mode <| state

    let private moveForward = moveForwardWith InputMode.Input

    let private setCursor
        (cursor: int)
        (mode: InputMode)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =
        let qs =
            QueryState.setCursor state.QueryState cursor |> QueryState.setInputMode mode

        state
        |> InternalState.refreshIfTrue (state.QueryState.Cursor <> cursor)
        |> InternalState.updateQueryState qs,
        pos,
        context

    let private moveHeadWith = setCursor 0
    let private moveHead = moveHeadWith InputMode.Input

    let private moveTailWith (mode: InputMode) (state: InternalState) =
        setCursor <| String.length state.QueryState.Query <| mode <| state

    let private moveTail = moveTailWith InputMode.Input

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type private Direction =
        | Backward
        | Forward

    let private removeChar
        (direction: Direction)
        (size: int)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =
        let limit =
            match direction with
            | Direction.Backward -> 0
            | Direction.Forward -> String.length state.QueryState.Query

        match state.QueryState.Cursor with
        | x when x = limit -> InternalState.noRefresh state, pos, context
        | _ ->
            let qs =
                match direction with
                | Direction.Backward -> QueryState.backspaceQuery state.QueryState
                | Direction.Forward -> QueryState.deleteQuery state.QueryState
                <| size

            let s =
                state
                |> InternalState.refreshIfTrue (
                    state.QueryState.Query <> qs.Query || state.QueryState.Cursor <> qs.Cursor
                )
                |> InternalState.updateQueryState qs
                |> InternalState.prepareNotification

            s, pos, context |> QueryContext.prepareQuery s

    let private removeBackwardChar = removeChar Direction.Backward 1

    let private removeForwardChar = removeChar Direction.Forward 1

    let private removeQueryHead (state: InternalState) =
        removeChar Direction.Backward state.QueryState.Cursor state

    let private removeQueryTail (state: InternalState) =
        removeChar Direction.Forward
        <| String.length state.QueryState.Query - state.QueryState.Cursor
        <| state

    let private selectBackwardChar (state: InternalState) =
        moveBackwardWith <| QueryState.getQuerySelection -1 state.QueryState <| state

    let private selectForwardChar (state: InternalState) =
        moveForwardWith <| QueryState.getQuerySelection 1 state.QueryState <| state

    let private selectToBeginningOfLine (state: InternalState) (pos: Position) (context: QueryContext) =
        setCursor 0
        <| (QueryState.getQuerySelection -state.QueryState.Cursor state.QueryState)
        <| state
        <| pos
        <| context

    let private selectToEndOfLine (state: InternalState) (pos: Position) (context: QueryContext) =
        let s = String.length state.QueryState.Query - state.QueryState.Cursor

        setCursor
        <| String.length state.QueryState.Query
        <| (QueryState.getQuerySelection s state.QueryState)
        <| state
        <| pos
        <| context

    let private switchMatcher (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            state
            |> InternalState.rotateMatcher
            |> InternalState.refresh
            |> InternalState.prepareNotification
            |> InternalState.updateWindowWidth

        state, pos, context |> QueryContext.prepareIs state

    let private switchOperator (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            state
            |> InternalState.rotateOperator
            |> InternalState.refresh
            |> InternalState.updateWindowWidth

        state, pos, context |> QueryContext.prepareQuery state |> QueryContext.prepareTest state

    let private toggleCaseSensitive (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            state
            |> InternalState.toggleCaseSensitive
            |> InternalState.refresh
            |> InternalState.updateWindowWidth

        state, pos, context |> QueryContext.prepareIs state

    let private toggleInvertFilter (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            state
            |> InternalState.toggleInvertFilter
            |> InternalState.refresh
            |> InternalState.updateWindowWidth

        state, pos, context |> QueryContext.prepareAnswer state

    let private toggleSuppressProperties (state: InternalState) (pos: Position) (context: QueryContext) =
        let state = state |> InternalState.toggleSuppressProperties |> InternalState.refresh

        state, pos, context

    let private (|AlreadyCompleted|_|) (keyword: string) (tail: string) (candidate: string) =
        let rest: string =
            match keyword with
            | "" -> candidate
            | _ -> candidate |> String.replace keyword ""

        let tailHead = String.split " " tail |> Seq.head

        match rest = tailHead with
        | true -> Some(tail.[String.length tailHead ..])
        | _ -> None

    let private completeProperty (state: InternalState) (pos: Position) (context: QueryContext) =
        let splitQuery keyword candidate =
            let basePosition = state.QueryState.Cursor - String.length keyword
            let head = state.QueryState.Query.[.. basePosition - 1]
            let tail = state.QueryState.Query.[state.QueryState.Cursor ..]

            match candidate with
            | AlreadyCompleted keyword tail rest -> basePosition, head, rest
            | _ -> basePosition, head, tail

        let buildValues head next tail keyword i candidates basePosition =
            let state =
                { state with
                    InternalState.QueryState.Query = $"%s{head}%s{next}%s{tail}"
                    InternalState.QueryState.Cursor = basePosition + String.length next
                    PropertySearch = PropertySearch.Rotate(keyword, i, candidates) }
                |> InternalState.refresh

            state, pos, context |> QueryContext.prepareQuery state

        match state.PropertySearch with
        | PropertySearch.NoSearch -> InternalState.noRefresh state, pos, context
        | PropertySearch.Search keyword ->
            let candidates =
                state.Properties
                |> List.filter (String.lower >> String.startsWith (String.lower keyword))

            match candidates with
            | [] -> InternalState.noRefresh state, pos, context
            | candidates ->
                let candidate = List.head candidates
                let basePosition, head, tail = splitQuery keyword candidate
#if DEBUG
                Logger.LogFile [ $"Search keyword '{keyword}' head '{head}' candidate '{candidate}' tail '{tail}'" ]
#endif
                buildValues head candidate tail keyword 0 candidates basePosition
        | PropertySearch.Rotate(keyword, i, candidates) ->
            let cur = candidates.[i]
            let i = (i + 1) % List.length candidates
            let next = candidates.[i]
            let basePosition, head, tail = splitQuery cur next
#if DEBUG
            Logger.LogFile [ $"Rotate keyword '{keyword}' head '{head}' cur '{cur}' next '{next}' tail '{tail}'" ]
#endif
            buildValues head next tail keyword i candidates basePosition

    let invokeAction (state: InternalState) (pos: Position) (context: QueryContext) (acton: Action) =
        match acton with
        | Action.Noop -> InternalState.noRefresh state, pos, context
        | Action.AddQuery s -> addQuery state pos context s
        | Action.BackwardChar -> moveBackward state pos context
        | Action.ForwardChar -> moveForward state pos context
        | Action.BeginningOfLine -> moveHead state pos context
        | Action.EndOfLine -> moveTail state pos context
        | Action.DeleteBackwardChar -> removeBackwardChar state pos context
        | Action.DeleteForwardChar -> removeForwardChar state pos context
        | Action.KillBeginningOfLine -> removeQueryHead state pos context
        | Action.KillEndOfLine -> removeQueryTail state pos context
        | Action.SelectBackwardChar -> selectBackwardChar state pos context
        | Action.SelectForwardChar -> selectForwardChar state pos context
        | Action.SelectToBeginningOfLine -> selectToBeginningOfLine state pos context
        | Action.SelectToEndOfLine -> selectToEndOfLine state pos context
        | Action.RotateMatcher -> switchMatcher state pos context
        | Action.RotateOperator -> switchOperator state pos context
        | Action.ToggleCaseSensitive -> toggleCaseSensitive state pos context
        | Action.ToggleInvertFilter -> toggleInvertFilter state pos context
        | Action.ToggleSuppressProperties -> toggleSuppressProperties state pos context
        | Action.SelectLineUp
        | Action.SelectLineDown
        | Action.ScrollPageUp
        | Action.ScrollPageDown -> InternalState.noRefresh state, pos, context // TODO: implement it.
        | Action.CompleteProperty -> completeProperty state pos context
        | x -> failwithf "unrecognized Action. value='%s'" <| x.GetType().Name
