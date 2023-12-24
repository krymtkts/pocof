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

        state, p, { context with Queries = prepareQuery state }, Required

    let private moveBackward (state: InternalState) (pos: Position) (context: QueryContext) =
        let p, changed =
            match pos.X with
            | 0 -> pos, NotRequired
            | _ -> { pos with X = pos.X - 1 }, Required

        { state with PropertySearch = getCurrentProperty state.Query <| p.X - 1 }, p, context, changed

    let private moveForward (state: InternalState) (pos: Position) (context: QueryContext) =
        let p, changed =
            match pos.X < state.Query.Length with
            | true -> { pos with X = pos.X + 1 }, Required
            | _ -> pos, NotRequired

        { state with PropertySearch = getCurrentProperty state.Query <| p.X - 1 }, p, context, changed

    let private moveHead (state: InternalState) (pos: Position) (context: QueryContext) =
        { state with PropertySearch = NoSearch },
        { pos with X = 0 },
        context,
        (pos.X <> 0 && state.PropertySearch <> NoSearch)
        |> Refresh.ofBool

    let private moveTail (state: InternalState) (pos: Position) (context: QueryContext) =
        let ps = getCurrentProperty state.Query state.Query.Length

        { state with PropertySearch = getCurrentProperty state.Query state.Query.Length },
        { pos with X = state.Query.Length },
        context,
        (pos.X <> state.Query.Length
         && ps <> state.PropertySearch)
        |> Refresh.ofBool

    let private removeBackwardChar (state: InternalState) (pos: Position) (context: QueryContext) =
        match pos.X with
        | 0 -> state, pos, context, NotRequired
        | _ ->
            let p = { pos with X = pos.X - 1 }

            let q =
                match state.Query.Length > p.X with
                | true -> state.Query.Remove(p.X, 1)
                | _ -> state.Query

            let state =
                { state with
                    Query = q
                    PropertySearch = getCurrentProperty q p.X }

            state, p, { context with Queries = prepareQuery state }, Required

    let private removeForwardChar (state: InternalState) (pos: Position) (context: QueryContext) =
        let q, changed =
            match state.Query.Length > pos.X with
            | true -> state.Query.Remove(pos.X, 1), Required
            | _ -> state.Query, NotRequired

        let state =
            { state with
                Query = q
                PropertySearch = getCurrentProperty q pos.X }

        state, pos, context, changed

    let private removeQueryHead (state: InternalState) (pos: Position) (context: QueryContext) =
        let q = state.Query.[pos.X ..]
        let ps = getCurrentProperty q 0

        let refresh =
            (q <> state.Query || ps <> state.PropertySearch)
            |> Refresh.ofBool

        let state =
            { state with
                Query = q
                PropertySearch = ps }

        state, { pos with X = 0 }, { context with Queries = prepareQuery state }, refresh

    let private removeQueryTail (state: InternalState) (pos: Position) (context: QueryContext) =
        let q = state.Query.[.. pos.X - 1]
        let ps = getCurrentProperty q pos.X

        let refresh =
            (q <> state.Query || ps <> state.PropertySearch)
            |> Refresh.ofBool

        let state =
            { state with
                Query = q
                PropertySearch = ps }

        state, pos, { context with Queries = prepareQuery state }, refresh

    let private switchMatcher (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            { state with
                InternalState.QueryState.Matcher =
                    match state.QueryState.Matcher with
                    | EQ -> LIKE
                    | LIKE -> MATCH
                    | MATCH -> EQ }

        state, pos, { context with Queries = prepareQuery state }, Required

    let private switchOperator (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            { state with
                InternalState.QueryState.Operator =
                    match state.QueryState.Operator with
                    | OR -> AND
                    | AND -> NONE
                    | NONE -> OR }

        state, pos, { context with Queries = prepareQuery state }, Required

    let private switchCaseSensitive (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            { state with InternalState.QueryState.CaseSensitive = not state.QueryState.CaseSensitive }

        state, pos, { context with Queries = prepareQuery state }, Required

    let private switchInvertFilter (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            { state with InternalState.QueryState.Invert = not state.QueryState.Invert }

        state, pos, { context with Queries = prepareQuery state }, Required

    let private switchSuppressProperties (state: InternalState) (pos: Position) (context: QueryContext) =
        let state = { state with SuppressProperties = not state.SuppressProperties }

        state, pos, { context with Queries = prepareQuery state }, Required

    let private completeProperty (state: InternalState) (pos: Position) (context: QueryContext) =
        let splitQuery keyword =
            let basePosition = pos.X - String.length keyword
            let head = state.Query.[.. basePosition - 1]
            let tail = state.Query.[pos.X ..]
            basePosition, head, tail

        let buildValues head next tail keyword i candidates basePosition =
            let state =
                { state with
                    Query = $"%s{head}%s{next}%s{tail}"
                    PropertySearch = Rotate(keyword, i, candidates) }

            state, { pos with X = basePosition + next.Length }, { context with Queries = prepareQuery state }, Required

        match state.PropertySearch with
        | NoSearch -> state, pos, context, NotRequired
        | Search keyword ->
            let candidate, candidates =
                state.Properties
                |> List.filter (
                    String.lower
                    >> String.startsWith (String.lower keyword)
                )
                |> function
                    | [] -> "", []
                    | xs -> List.head xs, xs

            match candidate with
            | "" -> state, pos, context, NotRequired
            | _ ->
                let basePosition, head, tail = splitQuery keyword
#if DEBUG
                Logger.logFile [ $"Search keyword '{keyword}' head '{head}' candidate '{candidate}' tail '{tail}'" ]
#endif
                buildValues head candidate tail keyword 0 candidates basePosition
        | Rotate (keyword, i, candidates) ->
            let cur = candidates.[i]
            let i = (i + 1) % candidates.Length
            let next = candidates.[i]
            let basePosition, head, tail = splitQuery cur
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
        | SelectUp -> state, pos, context, NotRequired // TODO: implement it.
        | SelectDown -> state, pos, context, NotRequired // TODO: implement it.
        | ScrollPageUp -> state, pos, context, NotRequired // TODO: implement it.
        | ScrollPageDown -> state, pos, context, NotRequired // TODO: implement it.
        | CompleteProperty -> completeProperty state pos context
        | x ->
            failwithf "unrecognized Action. value='%s'"
            <| x.GetType().Name
