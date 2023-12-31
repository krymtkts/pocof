namespace pocof

open System
open PocofData
open PocofQuery

module PocofHandle =
    type QueryContext = PocofQuery.QueryContext

    let private addQuery (state: InternalState) (pos: Position) (context: QueryContext) (s: string) =
        let query = state.Query.Insert(pos.X, s)
        let p = { pos with X = pos.X + String.length s }

        let state =
            { state with
                Query = query
                PropertySearch = getCurrentProperty query p.X }
            |> refresh

        let notification = prepareNotification state
        { state with Notification = notification }, p, { context with Queries = prepareQuery state }

    let private moveBackward (state: InternalState) (pos: Position) (context: QueryContext) =
        let p, refresh =
            match pos.X with
            | 0 -> pos, NotRequired
            | _ -> { pos with X = pos.X - 1 }, Required

        { state with
            PropertySearch = getCurrentProperty state.Query p.X
            Refresh = refresh },
        p,
        context

    let private moveForward (state: InternalState) (pos: Position) (context: QueryContext) =
        let p, refresh =
            match pos.X < state.Query.Length with
            | true -> { pos with X = pos.X + 1 }, Required
            | _ -> pos, NotRequired

        { state with
            PropertySearch = getCurrentProperty state.Query p.X
            Refresh = refresh },
        p,
        context

    let private moveHead (state: InternalState) (pos: Position) (context: QueryContext) =
        { state with
            PropertySearch = NoSearch
            Refresh = pos.X <> 0 |> Refresh.ofBool },
        { pos with X = 0 },
        context

    let private moveTail (state: InternalState) (pos: Position) (context: QueryContext) =
        { state with
            PropertySearch = getCurrentProperty state.Query state.Query.Length
            Refresh = pos.X <> state.Query.Length |> Refresh.ofBool },
        { pos with X = state.Query.Length },
        context

    let private removeBackwardChar (state: InternalState) (pos: Position) (context: QueryContext) =
        match pos.X with
        | 0 -> noRefresh state, pos, context
        | _ ->
            let x = pos.X - 1

            let q, x =
                match state.Query.Length > x with
                | true -> state.Query.Remove(x, 1), x
                | _ -> state.Query, state.Query.Length

            let s =
                { state with
                    Query = q
                    PropertySearch = getCurrentProperty q x }

            let notification = prepareNotification s

            { s with
                Notification = notification
                Refresh = state.Query <> q |> Refresh.ofBool },
            { pos with X = x },
            { context with Queries = prepareQuery s }

    let private removeForwardChar (state: InternalState) (pos: Position) (context: QueryContext) =
        let q, refresh =
            match state.Query.Length > pos.X with
            | true -> state.Query.Remove(pos.X, 1), Required
            | _ -> state.Query, NotRequired

        let state =
            { state with
                Query = q
                PropertySearch = getCurrentProperty q pos.X
                Refresh = refresh }

        let notification = prepareNotification state
        { state with Notification = notification }, pos, context

    let private removeQueryHead (state: InternalState) (pos: Position) (context: QueryContext) =
        let q = state.Query.[pos.X ..]
        let ps = getCurrentProperty q 0

        let refresh =
            (q <> state.Query || ps <> state.PropertySearch)
            |> Refresh.ofBool

        let state =
            { state with
                Query = q
                PropertySearch = ps
                Refresh = refresh }

        let notification = prepareNotification state
        { state with Notification = notification }, { pos with X = 0 }, { context with Queries = prepareQuery state }

    let private removeQueryTail (state: InternalState) (pos: Position) (context: QueryContext) =
        let q = state.Query.[.. pos.X - 1]
        let ps = getCurrentProperty q pos.X

        let refresh =
            (q <> state.Query || ps <> state.PropertySearch)
            |> Refresh.ofBool

        let state =
            { state with
                Query = q
                PropertySearch = ps
                Refresh = refresh }

        let notification = prepareNotification state
        { state with Notification = notification }, pos, { context with Queries = prepareQuery state }

    let private switchMatcher (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            { state with
                InternalState.QueryState.Matcher =
                    match state.QueryState.Matcher with
                    | EQ -> LIKE
                    | LIKE -> MATCH
                    | MATCH -> EQ
                Notification = prepareNotification state }
            |> refresh

        state, pos, { context with Is = prepareIs state }

    let private switchOperator (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            { state with
                InternalState.QueryState.Operator =
                    match state.QueryState.Operator with
                    | OR -> AND
                    | AND -> NONE
                    | NONE -> OR }
            |> refresh

        state,
        pos,
        { context with
            Queries = prepareQuery state
            Test = prepareTest state }

    let private switchCaseSensitive (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            { state with InternalState.QueryState.CaseSensitive = not state.QueryState.CaseSensitive }
            |> refresh

        state, pos, { context with Is = prepareIs state }

    let private switchInvertFilter (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            { state with InternalState.QueryState.Invert = not state.QueryState.Invert }
            |> refresh

        state, pos, { context with Answer = prepareAnswer state }

    let private switchSuppressProperties (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            { state with SuppressProperties = not state.SuppressProperties }
            |> refresh

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
            let basePosition = pos.X - String.length keyword
            let head = state.Query.[.. basePosition - 1]
            let tail = state.Query.[pos.X ..]

            match candidate with
            | AlreadyCompleted keyword tail rest -> basePosition, head, rest
            | _ -> basePosition, head, tail

        let buildValues head next tail keyword i candidates basePosition =
            let state =
                { state with
                    Query = $"%s{head}%s{next}%s{tail}"
                    PropertySearch = Rotate(keyword, i, candidates) }
                |> refresh

            state, { pos with X = basePosition + next.Length }, { context with Queries = prepareQuery state }

        match state.PropertySearch with
        | NoSearch -> noRefresh state, pos, context
        | Search keyword ->
            let candidates =
                state.Properties
                |> List.filter (
                    String.lower
                    >> String.startsWith (String.lower keyword)
                )

            match candidates with
            | [] -> noRefresh state, pos, context
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
        | SelectUp -> noRefresh state, pos, context // TODO: implement it.
        | SelectDown -> noRefresh state, pos, context // TODO: implement it.
        | ScrollPageUp -> noRefresh state, pos, context // TODO: implement it.
        | ScrollPageDown -> noRefresh state, pos, context // TODO: implement it.
        | CompleteProperty -> completeProperty state pos context
        | x ->
            failwithf "unrecognized Action. value='%s'"
            <| x.GetType().Name
