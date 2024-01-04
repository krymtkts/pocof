namespace pocof

open System
open PocofData
open PocofQuery

module PocofHandle =
    type QueryContext = PocofQuery.QueryContext

    let private addQuery (state: InternalState) (pos: Position) (context: QueryContext) (s: string) =
        let qs = QueryState.addQuery state.QueryState s

        let state =
            state
            |> InternalState.updateQueryState qs
            |> InternalState.refresh
            |> InternalState.prepareNotification

        state, pos, context |> QueryContext.prepareQuery state

    let private moveCursor (cursor: int) (limit: int) (state: InternalState) (pos: Position) (context: QueryContext) =
        let qs = QueryState.moveCursor state.QueryState cursor

        state
        |> InternalState.refreshIfTrue (state.QueryState.Cursor <> limit)
        |> InternalState.updateQueryState qs,
        pos,
        context

    let private moveBackward = moveCursor -1 0

    let private moveForward (state: InternalState) =
        moveCursor 1
        <| String.length state.QueryState.Query
        <| state

    let private setCursor (cursor: int) (state: InternalState) (pos: Position) (context: QueryContext) =
        let qs = QueryState.setCursor state.QueryState cursor

        state
        |> InternalState.refreshIfTrue (state.QueryState.Cursor <> cursor)
        |> InternalState.updateQueryState qs,
        pos,
        context

    let private moveHead = setCursor 0

    let private moveTail (state: InternalState) =
        setCursor
        <| String.length state.QueryState.Query
        <| state

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
            | Backward -> 0
            | Forward -> String.length state.QueryState.Query

        match state.QueryState.Cursor with
        | x when x = limit -> InternalState.noRefresh state, pos, context
        | _ ->
            let qs =
                match direction with
                | Backward -> QueryState.backspaceQuery state.QueryState
                | Forward -> QueryState.deleteQuery state.QueryState
                <| size

            let s =
                state
                |> InternalState.refreshIfTrue (
                    state.QueryState.Query <> qs.Query
                    || state.QueryState.Cursor <> qs.Cursor
                )
                |> InternalState.updateQueryState qs
                |> InternalState.prepareNotification

            s, pos, context |> QueryContext.prepareQuery s

    let private removeBackwardChar = removeChar Backward 1

    let private removeForwardChar = removeChar Forward 1

    let private removeQueryHead (state: InternalState) =
        removeChar Backward state.QueryState.Cursor state

    let private removeQueryTail (state: InternalState) =
        removeChar Forward
        <| String.length state.QueryState.Query
           - state.QueryState.Cursor
        <| state

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

        state,
        pos,
        context
        |> QueryContext.prepareQuery state
        |> QueryContext.prepareTest state

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
        let state =
            state
            |> InternalState.toggleSuppressProperties
            |> InternalState.refresh

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
                    PropertySearch = Rotate(keyword, i, candidates) }
                |> InternalState.refresh
                |> InternalState.adjustCursor

            state, pos, context |> QueryContext.prepareQuery state

        match state.PropertySearch with
        | NoSearch -> InternalState.noRefresh state, pos, context
        | Search keyword ->
            let candidates =
                state.Properties
                |> List.filter (
                    String.lower
                    >> String.startsWith (String.lower keyword)
                )

            match candidates with
            | [] -> InternalState.noRefresh state, pos, context
            | candidates ->
                let candidate = List.head candidates
                let basePosition, head, tail = splitQuery keyword candidate
#if DEBUG
                Logger.logFile [ $"Search keyword '{keyword}' head '{head}' candidate '{candidate}' tail '{tail}'" ]
#endif
                buildValues head candidate tail keyword 0 candidates basePosition
        | Rotate (keyword, i, candidates) ->
            let cur = candidates.[i]
            let i = (i + 1) % List.length candidates
            let next = candidates.[i]
            let basePosition, head, tail = splitQuery cur next
#if DEBUG
            Logger.logFile [ $"Rotate keyword '{keyword}' head '{head}' cur '{cur}' next '{next}' tail '{tail}'" ]
#endif
            buildValues head next tail keyword i candidates basePosition

    let invokeAction (state: InternalState) (pos: Position) (context: QueryContext) =
        function
        | AddQuery s -> addQuery state pos context s
        | BackwardChar -> moveBackward state pos context
        | ForwardChar -> moveForward state pos context
        | BeginningOfLine -> moveHead state pos context
        | EndOfLine -> moveTail state pos context
        | DeleteBackwardChar -> removeBackwardChar state pos context
        | DeleteForwardChar -> removeForwardChar state pos context
        | KillBeginningOfLine -> removeQueryHead state pos context
        | KillEndOfLine -> removeQueryTail state pos context
        | RotateMatcher -> switchMatcher state pos context
        | RotateOperator -> switchOperator state pos context
        | ToggleCaseSensitive -> toggleCaseSensitive state pos context
        | ToggleInvertFilter -> toggleInvertFilter state pos context
        | ToggleSuppressProperties -> toggleSuppressProperties state pos context
        | SelectUp -> InternalState.noRefresh state, pos, context // TODO: implement it.
        | SelectDown -> InternalState.noRefresh state, pos, context // TODO: implement it.
        | ScrollPageUp -> InternalState.noRefresh state, pos, context // TODO: implement it.
        | ScrollPageDown -> InternalState.noRefresh state, pos, context // TODO: implement it.
        | CompleteProperty -> completeProperty state pos context
        | x ->
            failwithf "unrecognized Action. value='%s'"
            <| x.GetType().Name
