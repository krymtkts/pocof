namespace pocof


module PocofHandle =
    open System

    open PocofData

    let private addQuery (state: InternalState) (pos: Position) (s: string) =
        let query = state.Query.Insert(pos.X, s)
        let p = { pos with X = pos.X + String.length s }

        { state with
            Query = query
            PropertySearch = getCurrentProperty query p.X },
        p,
        Required

    let private moveBackward (state: InternalState) (pos: Position) =
        let p, changed =
            match pos.X with
            | 0 -> pos, NotRequired
            | _ -> { pos with X = pos.X - 1 }, Required

        { state with PropertySearch = getCurrentProperty state.Query <| p.X - 1 }, p, changed

    let private moveForward (state: InternalState) (pos: Position) =
        let p, changed =
            match pos.X < state.Query.Length with
            | true -> { pos with X = pos.X + 1 }, Required
            | _ -> pos, NotRequired

        { state with PropertySearch = getCurrentProperty state.Query <| p.X - 1 }, p, changed

    let private moveHead (state: InternalState) (pos: Position) =
        { state with PropertySearch = NoSearch },
        { pos with X = 0 },
        (pos.X <> 0 && state.PropertySearch <> NoSearch)
        |> Refresh.ofBool

    let private moveTail (state: InternalState) (pos: Position) =
        let ps = getCurrentProperty state.Query state.Query.Length

        { state with PropertySearch = getCurrentProperty state.Query state.Query.Length },
        { pos with X = state.Query.Length },
        (pos.X <> state.Query.Length
         && ps <> state.PropertySearch)
        |> Refresh.ofBool

    let private removeBackwardChar (state: InternalState) (pos: Position) =
        match pos.X with
        | 0 -> state, pos, NotRequired
        | _ ->
            let p = { pos with X = pos.X - 1 }

            let q =
                match state.Query.Length > p.X with
                | true -> state.Query.Remove(p.X, 1)
                | _ -> state.Query

            { state with
                Query = q
                PropertySearch = getCurrentProperty q p.X },
            p,
            Required

    let private removeForwardChar (state: InternalState) (pos: Position) =
        let q, changed =
            match state.Query.Length > pos.X with
            | true -> state.Query.Remove(pos.X, 1), Required
            | _ -> state.Query, NotRequired

        { state with
            Query = q
            PropertySearch = getCurrentProperty q pos.X },
        pos,
        changed

    let private removeQueryHead (state: InternalState) (pos: Position) =
        let q = state.Query.[pos.X ..]
        let ps = getCurrentProperty q 0

        { state with
            Query = q
            PropertySearch = ps },
        { pos with X = 0 },
        (q <> state.Query || ps <> state.PropertySearch)
        |> Refresh.ofBool

    let private removeQueryTail (state: InternalState) (pos: Position) =
        let q = state.Query.[.. pos.X - 1]
        let ps = getCurrentProperty q pos.X

        { state with
            Query = q
            PropertySearch = ps },
        pos,
        (q <> state.Query || ps <> state.PropertySearch)
        |> Refresh.ofBool

    let private switchMatcher (state: InternalState) =
        { state with
            InternalState.QueryState.Matcher =
                match state.QueryState.Matcher with
                | EQ -> LIKE
                | LIKE -> MATCH
                | MATCH -> EQ }

    let private switchOperator (state: InternalState) =
        { state with
            InternalState.QueryState.Operator =
                match state.QueryState.Operator with
                | OR -> AND
                | AND -> NONE
                | NONE -> OR }

    let private switchCaseSensitive (state: InternalState) =
        { state with InternalState.QueryState.CaseSensitive = not state.QueryState.CaseSensitive }

    let private switchInvertFilter (state: InternalState) =
        { state with InternalState.QueryState.Invert = not state.QueryState.Invert }

    let private switchSuppressProperties (state: InternalState) =
        { state with SuppressProperties = not state.SuppressProperties }

    let private completeProperty (state: InternalState) (pos: Position) (props: string list) =
        let splitQuery keyword =
            let basePosition = pos.X - String.length keyword
            let head = state.Query.[.. basePosition - 1]
            let tail = state.Query.[pos.X ..]
            basePosition, head, tail

        let buildValues head next tail keyword i candidates basePosition =
            { state with
                Query = $"%s{head}%s{next}%s{tail}"
                PropertySearch = Rotate(keyword, i, candidates) },
            { pos with X = basePosition + next.Length },
            Required

        match state.PropertySearch with
        | NoSearch -> state, pos, NotRequired
        | Search keyword ->
            let candidate, candidates =
                props
                |> List.filter (
                    String.lower
                    >> String.startsWith (String.lower keyword)
                )
                |> function
                    | [] -> "", []
                    | xs -> List.head xs, xs

            match candidate with
            | "" -> state, pos, NotRequired
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

    let invokeAction (state: InternalState) (pos: Position) (props: string list) =
        function
        | AddQuery s -> addQuery state pos s
        | BackwardChar -> moveBackward state pos
        | ForwardChar -> moveForward state pos
        | BeginningOfLine -> moveHead state pos
        | EndOfLine -> moveTail state pos
        | DeleteBackwardChar -> removeBackwardChar state pos
        | DeleteForwardChar -> removeForwardChar state pos
        | KillBeginningOfLine -> removeQueryHead state pos
        | KillEndOfLine -> removeQueryTail state pos
        | RotateMatcher -> switchMatcher state, pos, Required
        | RotateOperator -> switchOperator state, pos, Required
        | ToggleCaseSensitive -> switchCaseSensitive state, pos, Required
        | ToggleInvertFilter -> switchInvertFilter state, pos, Required
        | ToggleSuppressProperties -> switchSuppressProperties state, pos, Required
        | SelectUp -> state, pos, NotRequired // TODO: implement it.
        | SelectDown -> state, pos, NotRequired // TODO: implement it.
        | ScrollPageUp -> state, pos, NotRequired // TODO: implement it.
        | ScrollPageDown -> state, pos, NotRequired // TODO: implement it.
        // TODO: i think it is not good to include props in invokeAction only for completion.
        | CompleteProperty -> completeProperty state pos props
        | x ->
            failwithf "unrecognized Action. value='%s'"
            <| x.GetType().Name
