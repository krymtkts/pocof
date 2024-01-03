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

        state, pos, { context with Queries = prepareQuery state }

    let private moveBackward (state: InternalState) (pos: Position) (context: QueryContext) =
        let qs = QueryState.moveCursor state.QueryState -1

        let refresh =
            match state.QueryState.Cursor with
            | 0 -> NotRequired
            | _ -> Required

        { state with Refresh = refresh }
        |> InternalState.updateQueryState qs,
        pos,
        context

    let private moveForward (state: InternalState) (pos: Position) (context: QueryContext) =
        let qs = QueryState.moveCursor state.QueryState 1

        let refresh =
            match state.QueryState.Cursor < String.length state.QueryState.Query with
            | true -> Required
            | _ -> NotRequired

        { state with Refresh = refresh }
        |> InternalState.updateQueryState qs,
        pos,
        context

    let private moveHead (state: InternalState) (pos: Position) (context: QueryContext) =
        let qs = QueryState.setCursor state.QueryState 0

        { state with Refresh = state.QueryState.Cursor <> 0 |> Refresh.ofBool }
        |> InternalState.updateQueryState qs,
        pos,
        context

    let private moveTail (state: InternalState) (pos: Position) (context: QueryContext) =
        let l = String.length state.QueryState.Query
        let qs = QueryState.setCursor state.QueryState l

        { state with Refresh = state.QueryState.Cursor <> l |> Refresh.ofBool }
        |> InternalState.updateQueryState qs,
        pos,
        context

    let private removeBackwardChar (state: InternalState) (pos: Position) (context: QueryContext) =
        match state.QueryState.Cursor with
        | 0 -> InternalState.noRefresh state, pos, context
        | _ ->
            let qs = QueryState.backspaceQuery state.QueryState 1

            let s =
                { state with
                    Refresh =
                        (state.QueryState.Query <> qs.Query
                         || state.QueryState.Cursor <> qs.Cursor)
                        |> Refresh.ofBool }
                |> InternalState.updateQueryState qs
                |> InternalState.prepareNotification

            s, pos, { context with Queries = prepareQuery s }

    let private removeForwardChar (state: InternalState) (pos: Position) (context: QueryContext) =
        match state.QueryState.Cursor with
        | x when x = String.length state.QueryState.Query -> InternalState.noRefresh state, pos, context
        | _ ->
            let qs = QueryState.deleteQuery state.QueryState 1

            let s =
                { state with
                    Refresh =
                        (state.QueryState.Query <> qs.Query
                         || state.QueryState.Cursor <> qs.Cursor)
                        |> Refresh.ofBool }
                |> InternalState.updateQueryState qs
                |> InternalState.prepareNotification

            s, pos, { context with Queries = prepareQuery s }

    let private removeQueryHead (state: InternalState) (pos: Position) (context: QueryContext) =
        match state.QueryState.Cursor with
        | 0 -> InternalState.noRefresh state, pos, context
        | _ ->
            let qs = QueryState.backspaceQuery state.QueryState state.QueryState.Cursor

            let s =
                { state with
                    Refresh =
                        state.QueryState.Query <> qs.Query
                        |> Refresh.ofBool }
                |> InternalState.updateQueryState qs
                |> InternalState.prepareNotification

            s, pos, { context with Queries = prepareQuery s }

    let private removeQueryTail (state: InternalState) (pos: Position) (context: QueryContext) =
        let l = String.length state.QueryState.Query

        match state.QueryState.Cursor with
        | x when x = l -> InternalState.noRefresh state, pos, context
        | _ ->
            let qs =
                QueryState.deleteQuery state.QueryState
                <| l - state.QueryState.Cursor

            let s =
                { state with
                    Refresh =
                        state.QueryState.Query <> qs.Query
                        |> Refresh.ofBool }
                |> InternalState.updateQueryState qs
                |> InternalState.prepareNotification

            s, pos, { context with Queries = prepareQuery s }

    let private switchMatcher (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            { state with
                InternalState.QueryCondition.Matcher =
                    match state.QueryCondition.Matcher with
                    | EQ -> LIKE
                    | LIKE -> MATCH
                    | MATCH -> EQ }
            |> InternalState.refresh
            |> InternalState.prepareNotification
            |> InternalState.updateWindowWidth

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
            |> InternalState.updateWindowWidth

        state,
        pos,
        { context with
            Queries = prepareQuery state
            Test = prepareTest state }

    let private toggleCaseSensitive (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            state
            |> InternalState.toggleCaseSensitive
            |> InternalState.refresh
            |> InternalState.updateWindowWidth

        state, pos, { context with Is = prepareIs state }

    let private toggleInvertFilter (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            state
            |> InternalState.toggleInvertFilter
            |> InternalState.refresh
            |> InternalState.updateWindowWidth

        state, pos, { context with Answer = prepareAnswer state }

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
