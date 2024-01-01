namespace pocof

open System
open PocofData
open PocofQuery

module PocofHandle =
    type QueryContext = PocofQuery.QueryContext

    let private addQuery (state: InternalState) (pos: Position) (context: QueryContext) (s: string) =
        let qs = QueryState.addQuery state.QueryState s

        let state =
            { state with
                QueryState = qs
                PropertySearch = QueryState.getCurrentProperty qs }
            |> InternalState.refresh

        let notification = prepareNotification state
        { state with Notification = notification }, pos, { context with Queries = prepareQuery state }

    let private moveBackward (state: InternalState) (pos: Position) (context: QueryContext) =
        let qs = QueryState.moveCursor state.QueryState -1

        let refresh =
            match state.QueryState.Cursor with
            | 0 -> NotRequired
            | _ -> Required

        { state with
            QueryState = qs
            PropertySearch = QueryState.getCurrentProperty qs
            Refresh = refresh },
        pos,
        context

    let private moveForward (state: InternalState) (pos: Position) (context: QueryContext) =
        let qs = QueryState.moveCursor state.QueryState 1

        let refresh =
            match state.QueryState.Cursor < state.QueryState.Query.Length with
            | true -> Required
            | _ -> NotRequired

        { state with
            QueryState = qs
            PropertySearch = QueryState.getCurrentProperty qs
            Refresh = refresh },
        pos,
        context

    let private moveHead (state: InternalState) (pos: Position) (context: QueryContext) =
        let qs = QueryState.setCursor state.QueryState 0

        { state with
            QueryState = qs
            PropertySearch = NoSearch
            Refresh = state.QueryState.Cursor <> 0 |> Refresh.ofBool },
        pos,
        context

    let private moveTail (state: InternalState) (pos: Position) (context: QueryContext) =
        let qs = QueryState.setCursor state.QueryState state.QueryState.Query.Length

        { state with
            QueryState = qs
            PropertySearch = QueryState.getCurrentProperty qs
            Refresh =
                state.QueryState.Cursor
                <> state.QueryState.Query.Length
                |> Refresh.ofBool },
        pos,
        context

    let private removeBackwardChar (state: InternalState) (pos: Position) (context: QueryContext) =
        match state.QueryState.Cursor with
        | 0 -> InternalState.noRefresh state, pos, context
        | _ ->
            let qs = QueryState.backspaceQuery state.QueryState 1

            let s =
                { state with
                    QueryState = qs
                    PropertySearch = QueryState.getCurrentProperty qs }

            let notification = prepareNotification s

            { s with
                Notification = notification
                Refresh =
                    state.QueryState.Query <> qs.Query
                    |> Refresh.ofBool },
            pos,
            { context with Queries = prepareQuery s }

    let private removeForwardChar (state: InternalState) (pos: Position) (context: QueryContext) =
        match state.QueryState.Cursor with
        | x when x = state.QueryState.Query.Length -> InternalState.noRefresh state, pos, context
        | _ ->
            let qs = QueryState.deleteQuery state.QueryState 1

            let s =
                { state with
                    QueryState = qs
                    PropertySearch = QueryState.getCurrentProperty qs }

            let notification = prepareNotification s

            { s with
                Notification = notification
                Refresh =
                    state.QueryState.Query <> qs.Query
                    |> Refresh.ofBool },
            pos,
            { context with Queries = prepareQuery s }

    let private removeQueryHead (state: InternalState) (pos: Position) (context: QueryContext) =
        match state.QueryState.Cursor with
        | 0 -> InternalState.noRefresh state, pos, context
        | _ ->
            let qs = QueryState.backspaceQuery state.QueryState state.QueryState.Cursor

            let s =
                { state with
                    QueryState = qs
                    PropertySearch = QueryState.getCurrentProperty qs }

            let notification = prepareNotification s

            { s with
                Notification = notification
                Refresh =
                    state.QueryState.Query <> qs.Query
                    |> Refresh.ofBool },
            pos,
            { context with Queries = prepareQuery s }

    let private removeQueryTail (state: InternalState) (pos: Position) (context: QueryContext) =
        match state.QueryState.Cursor with
        | x when x = state.QueryState.Query.Length -> InternalState.noRefresh state, pos, context
        | _ ->
            let qs =
                QueryState.deleteQuery
                    state.QueryState
                    (state.QueryState.Query.Length
                     - state.QueryState.Cursor)

            let s =
                { state with
                    QueryState = qs
                    PropertySearch = QueryState.getCurrentProperty qs }

            let notification = prepareNotification s

            { s with
                Notification = notification
                Refresh =
                    state.QueryState.Query <> qs.Query
                    |> Refresh.ofBool },
            pos,
            { context with Queries = prepareQuery s }

    let private switchMatcher (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            { state with
                InternalState.QueryCondition.Matcher =
                    match state.QueryCondition.Matcher with
                    | EQ -> LIKE
                    | LIKE -> MATCH
                    | MATCH -> EQ
                Notification = prepareNotification state }
            |> InternalState.refresh

        state, pos, { context with Is = prepareIs state }

    let private switchOperator (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            { state with
                InternalState.QueryCondition.Operator =
                    match state.QueryCondition.Operator with
                    | OR -> AND
                    | AND -> NONE
                    | NONE -> OR }
            |> InternalState.refresh

        state,
        pos,
        { context with
            Queries = prepareQuery state
            Test = prepareTest state }

    let private switchCaseSensitive (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            { state with InternalState.QueryCondition.CaseSensitive = not state.QueryCondition.CaseSensitive }
            |> InternalState.refresh

        state, pos, { context with Is = prepareIs state }

    let private switchInvertFilter (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            { state with InternalState.QueryCondition.Invert = not state.QueryCondition.Invert }
            |> InternalState.refresh

        state, pos, { context with Answer = prepareAnswer state }

    let private switchSuppressProperties (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            { state with SuppressProperties = not state.SuppressProperties }
            |> InternalState.refresh

        state, pos, context

    let private (|AlreadyCompleted|_|) (keyword: string) (tail: string) (candidate: string) =
        let rest: string =
            match keyword with
            | "" -> candidate
            | _ -> candidate |> String.replace keyword ""

        let tailHead = String.split " " tail |> Seq.head

        match rest = tailHead with
        | true -> Some(tail.[tailHead.Length ..])
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
                    InternalState.QueryState.Cursor = basePosition + next.Length
                    PropertySearch = Rotate(keyword, i, candidates) }
                |> InternalState.refresh

            state, pos, { context with Queries = prepareQuery state }

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
            let i = (i + 1) % candidates.Length
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
        | ToggleCaseSensitive -> switchCaseSensitive state pos context
        | ToggleInvertFilter -> switchInvertFilter state pos context
        | ToggleSuppressProperties -> switchSuppressProperties state pos context
        | SelectUp -> InternalState.noRefresh state, pos, context // TODO: implement it.
        | SelectDown -> InternalState.noRefresh state, pos, context // TODO: implement it.
        | ScrollPageUp -> InternalState.noRefresh state, pos, context // TODO: implement it.
        | ScrollPageDown -> InternalState.noRefresh state, pos, context // TODO: implement it.
        | CompleteProperty -> completeProperty state pos context
        | x ->
            failwithf "unrecognized Action. value='%s'"
            <| x.GetType().Name
