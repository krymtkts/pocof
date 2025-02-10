namespace Pocof

open System

open Data
open Query

module Handle =
    type QueryContext = Query.QueryContext

    let private addQuery (state: InternalState) (context: QueryContext) (query: string) =
        let qs = QueryState.deleteSelection state.QueryState |> QueryState.addQuery query

        let state = state |> InternalState.updateQueryState qs |> InternalState.refresh

        state, context |> QueryContext.prepareQuery state

    let private updateCursor
        (update: QueryState -> int -> QueryState)
        (cursor: int)
        (mode: InputMode)
        (state: InternalState)
        (context: QueryContext)
        =
        let qs = update state.QueryState cursor |> QueryState.setInputMode mode

        state
        |> InternalState.refreshIfTrue (state.QueryState.Cursor <> qs.Cursor)
        |> InternalState.updateQueryState qs,
        context

    let private moveCursor = updateCursor QueryState.moveCursor
    let private moveCursorBackwardWith = moveCursor -1
    let private backwardChar = moveCursorBackwardWith InputMode.Input
    let private moveCursorForwardWith (mode: InputMode) (state: InternalState) = moveCursor 1 mode state
    let private forwardChar = moveCursorForwardWith InputMode.Input

    [<Literal>]
    let wordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"

    let private isWordDelimiter (wordDelimiters: string) (c: char) =
        Char.IsWhiteSpace c || wordDelimiters.IndexOf(c) >= 0

    [<TailCall>]
    let rec private findCursorOfChar (predicate: char -> bool) (str: char list) (cursor: int) =
        match str with
        | [] -> List.Empty, cursor
        | c :: cs ->
            if predicate c then
                cursor + 1 |> findCursorOfChar predicate cs
            else
                str, cursor

    let private findWordCursor wordDelimiters =
        isWordDelimiter wordDelimiters |> findCursorOfChar

    let rec private findWordDelimiterCursor wordDelimiters =
        isWordDelimiter wordDelimiters >> not |> findCursorOfChar

    let private findWordCursorWith substring findA findB (wordDelimiters: string) (query: string) (cursor: int) =
        if String.length query < cursor then
            List.Empty, 0
        else
            let str = substring cursor query |> List.ofSeq
            // NOTE: emulate the behavior of the backward-word function in the PSReadLine.
            findA wordDelimiters str 0 ||> findB wordDelimiters

    let private findBackwardWordCursor =
        findWordCursorWith (fun i s -> s |> String.upToIndex i |> Seq.rev) findWordCursor findWordDelimiterCursor

    let private findForwardWordCursor =
        findWordCursorWith String.fromIndex findWordDelimiterCursor findWordCursor

    let private wordAction (findWordCursor) (converter) (state: InternalState) =
        let i =
            findWordCursor state.QueryState.Query state.QueryState.Cursor
            |> snd
            |> converter

        moveCursor i InputMode.Input state

    let private backwardWord wordDelimiters =
        wordAction (findBackwardWordCursor wordDelimiters) (~-)

    let private forwardWord wordDelimiters =
        wordAction (findForwardWordCursor wordDelimiters) id

    let private setCursor = updateCursor QueryState.setCursor
    let private beginningOfLine = setCursor 0 InputMode.Input

    let private endOfLine (state: InternalState) =
        setCursor <| String.length state.QueryState.Query <| InputMode.Input <| state

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<NoEquality>]
    [<Struct>]
    type private Direction =
        | Backward
        | Forward

    let private removeSelection (state: InternalState) (context: QueryContext) =
        let qs = QueryState.deleteSelection state.QueryState

        let state = state |> InternalState.refresh |> InternalState.updateQueryState qs

        state, context |> QueryContext.prepareQuery state

    let private removeChars (direction: Direction) (size: int) (state: InternalState) (context: QueryContext) =
        let removeQuery, limit =
            match direction with
            | Direction.Backward -> QueryState.backspaceQuery, 0
            | Direction.Forward -> QueryState.deleteQuery, String.length state.QueryState.Query

        match state.QueryState.Cursor with
        | x when x = limit -> InternalState.noRefresh state, context
        | _ ->
            let qs = removeQuery state.QueryState size

            let state =
                state
                |> InternalState.refreshIfTrue (
                    state.QueryState.Query <> qs.Query || state.QueryState.Cursor <> qs.Cursor
                )
                |> InternalState.updateQueryState qs

            state, context |> QueryContext.prepareQuery state

    let private removeCharsWithInputMode
        (direction: Direction)
        (size: int)
        (state: InternalState)
        (context: QueryContext)
        =
        match state.QueryState.InputMode with
        | InputMode.Select _ -> removeSelection state context
        | _ -> removeChars direction size state context

    let private deleteBackwardChar = removeCharsWithInputMode Direction.Backward 1
    let private deleteForwardChar = removeCharsWithInputMode Direction.Forward 1

    let private expandSelection
        (getSelection: int -> int -> int)
        _ // NOTE: to match the signature of the calculateRemovalSize function.
        (selection: int)
        (wordCursor: int)
        (state: InternalState)
        (context: QueryContext)
        =
        let mode = getSelection selection wordCursor |> InputMode.Select

        let state =
            InternalState.updateQueryState
            <| QueryState.setInputMode mode state.QueryState
            <| state

        removeSelection state context

    let private calculateRemovalSize
        (op: int -> int -> int)
        (direction: Direction)
        (selection: int)
        (wordCursor: int)
        (state: InternalState)
        (context: QueryContext)
        =
        let cursor = state.QueryState.Cursor - selection
        let state, context = setCursor cursor InputMode.Input state context
        removeChars direction (op wordCursor selection) state context

    let private deleteWord
        findCursor
        direction
        handleBackwardSelection
        handleForwardSelection
        (state: InternalState)
        (context: QueryContext)
        =
        let wordCursor = findCursor state.QueryState.Query state.QueryState.Cursor |> snd

        match state.QueryState.InputMode with
        | Input -> removeChars direction wordCursor state context
        | SelectBackward selection -> handleBackwardSelection direction selection wordCursor state context
        | SelectForward selection -> handleForwardSelection direction selection wordCursor state context

    let private deleteBackwardWord wordDelimiters =
        deleteWord (findBackwardWordCursor wordDelimiters) Direction.Backward
        <| expandSelection max
        <| calculateRemovalSize (-)

    let private deleteForwardWord wordDelimiters =
        deleteWord (findForwardWordCursor wordDelimiters) Direction.Forward
        <| calculateRemovalSize (+)
        <| expandSelection (fun x y -> min x -y)

    let private deleteBackwardInput (state: InternalState) (context: QueryContext) =
        let state, context =
            match state.QueryState.InputMode with
            | InputMode.Input -> (state, context)
            | InputMode.Select c ->
                let selection = max state.QueryState.Cursor <| state.QueryState.Cursor - c

                setCursor selection InputMode.Input state context

        removeCharsWithInputMode Direction.Backward state.QueryState.Cursor state context

    let private deleteForwardInput (state: InternalState) (context: QueryContext) =
        let queryLength = String.length state.QueryState.Query

        let state, context, beginning =
            match state.QueryState.InputMode with
            | InputMode.Input -> (state, context, state.QueryState.Cursor)
            | InputMode.Select c ->
                let beginning = min state.QueryState.Cursor <| state.QueryState.Cursor - c

                let state, context = setCursor beginning InputMode.Input state context
                state, context, beginning

        removeCharsWithInputMode Direction.Forward (queryLength - beginning) state context

    let private selectBackwardChar (state: InternalState) =
        moveCursorBackwardWith
        <| QueryState.getQuerySelection -1 state.QueryState
        <| state

    let private selectForwardChar (state: InternalState) =
        moveCursorForwardWith
        <| QueryState.getQuerySelection 1 state.QueryState
        <| state

    let private selectWord findCursor operator converter (state: InternalState) =
        let i = findCursor state.QueryState.Query state.QueryState.Cursor |> snd

        setCursor
        <| operator state.QueryState.Cursor i
        <| QueryState.getQuerySelection (converter i) state.QueryState
        <| state

    let private selectBackwardWord wordDelimiters =
        selectWord (findBackwardWordCursor wordDelimiters) (-) (~-)

    let private selectForwardWord wordDelimiters =
        selectWord (findForwardWordCursor wordDelimiters) (+) id

    let private selectToBeginningOfLine (state: InternalState) (context: QueryContext) =
        setCursor 0
        <| QueryState.getQuerySelection -state.QueryState.Cursor state.QueryState
        <| state
        <| context

    let private selectToEndOfLine (state: InternalState) (context: QueryContext) =
        let s = String.length state.QueryState.Query - state.QueryState.Cursor

        setCursor
        <| String.length state.QueryState.Query
        <| QueryState.getQuerySelection s state.QueryState
        <| state
        <| context

    let private selectAll (state: InternalState) (context: QueryContext) =
        let s = String.length state.QueryState.Query

        let qs =
            QueryState.setInputMode
            <| InputMode.Select s
            <| QueryState.setCursor state.QueryState s

        state |> InternalState.refresh |> InternalState.updateQueryState qs, context

    let private rotateMatcher (state: InternalState) (context: QueryContext) =
        let state = state |> InternalState.rotateMatcher |> InternalState.refresh

        state, context |> QueryContext.prepareQuery state

    let private rotateOperator (state: InternalState) (context: QueryContext) =
        let state = state |> InternalState.rotateOperator |> InternalState.refresh

        state, context |> QueryContext.prepareQuery state |> QueryContext.prepareTest state

    let private toggleCaseSensitive (state: InternalState) (context: QueryContext) =
        let state = state |> InternalState.toggleCaseSensitive |> InternalState.refresh

        state, context |> QueryContext.prepareQuery state

    let private toggleInvertFilter (state: InternalState) (context: QueryContext) =
        let state = state |> InternalState.toggleInvertFilter |> InternalState.refresh

        state, context |> QueryContext.prepareQuery state

    let private toggleSuppressProperties (state: InternalState) (context: QueryContext) =
        let state = state |> InternalState.toggleSuppressProperties |> InternalState.refresh

        state, context

    let private (|AlreadyCompleted|_|) (keyword: string) (tail: string) (candidate: string) =
        let rest: string =
            match keyword with
            | "" -> candidate
            | _ -> candidate |> String.replace keyword ""

        let tailHead =
            let i = tail.IndexOf " "
            if i = -1 then tail else String.upToIndex i tail

        match rest = tailHead with
        | true -> tail |> String.fromIndex (String.length tailHead) |> Some
        | _ -> None

    let private completeProperty (properties: string seq) (state: InternalState) (context: QueryContext) =
        let splitQuery keyword candidate =
            let basePosition = state.QueryState.Cursor - String.length keyword
            let head = state.QueryState.Query |> String.upToIndex basePosition
            let tail = state.QueryState.Query |> String.fromIndex state.QueryState.Cursor

            match candidate with
            | AlreadyCompleted keyword tail rest -> basePosition, head, rest
            | _ -> basePosition, head, tail

        let buildValues head next tail keyword candidates basePosition =
            let state =
                { state with
                    InternalState.QueryState.Query = $"%s{head}%s{next}%s{tail}"
                    InternalState.QueryState.Cursor = basePosition + String.length next
                    PropertySearch = PropertySearch.Rotate(keyword, candidates) }
                |> InternalState.refresh

            state, context |> QueryContext.prepareQuery state

        match state.PropertySearch with
        | PropertySearch.NoSearch -> InternalState.noRefresh state, context
        | PropertySearch.Search keyword ->
            let candidates = properties |> Seq.filter (String.startsWithIgnoreCase keyword)

            match candidates |> Seq.isEmpty with
            | true -> InternalState.noRefresh state, context
            | _ ->
                let candidate = Seq.head candidates
                let basePosition, head, tail = splitQuery keyword candidate
#if DEBUG
                Logger.LogFile [ $"Search keyword '{keyword}' head '{head}' candidate '{candidate}' tail '{tail}'" ]
#endif
                buildValues head candidate tail keyword (candidates |> Seq.cycle) basePosition
        | PropertySearch.Rotate(keyword, candidates) ->
            let cur = candidates |> Seq.head
            let candidates = candidates |> Seq.tail
            let next = candidates |> Seq.head
            let basePosition, head, tail = splitQuery cur next
#if DEBUG
            Logger.LogFile [ $"Rotate keyword '{keyword}' head '{head}' cur '{cur}' next '{next}' tail '{tail}'" ]
#endif
            buildValues head next tail keyword candidates basePosition

    let invokeAction
        (wordDelimiters: string)
        (properties: string seq)
        (state: InternalState)
        (context: QueryContext)
        (action: Action)
        =
        match action with
        | Action.Noop -> InternalState.noRefresh state, context
        | Action.AddQuery query -> addQuery state context query
        | Action.BackwardChar -> backwardChar state context
        | Action.ForwardChar -> forwardChar state context
        | Action.BackwardWord -> backwardWord wordDelimiters state context
        | Action.ForwardWord -> forwardWord wordDelimiters state context
        | Action.BeginningOfLine -> beginningOfLine state context
        | Action.EndOfLine -> endOfLine state context
        | Action.DeleteBackwardChar -> deleteBackwardChar state context
        | Action.DeleteForwardChar -> deleteForwardChar state context
        | Action.DeleteBackwardWord -> deleteBackwardWord wordDelimiters state context
        | Action.DeleteForwardWord -> deleteForwardWord wordDelimiters state context
        | Action.DeleteBackwardInput -> deleteBackwardInput state context
        | Action.DeleteForwardInput -> deleteForwardInput state context
        | Action.SelectBackwardChar -> selectBackwardChar state context
        | Action.SelectForwardChar -> selectForwardChar state context
        | Action.SelectBackwardWord -> selectBackwardWord wordDelimiters state context
        | Action.SelectForwardWord -> selectForwardWord wordDelimiters state context
        | Action.SelectToBeginningOfLine -> selectToBeginningOfLine state context
        | Action.SelectToEndOfLine -> selectToEndOfLine state context
        | Action.SelectAll -> selectAll state context
        | Action.RotateMatcher -> rotateMatcher state context
        | Action.RotateOperator -> rotateOperator state context
        | Action.ToggleCaseSensitive -> toggleCaseSensitive state context
        | Action.ToggleInvertFilter -> toggleInvertFilter state context
        | Action.ToggleSuppressProperties -> toggleSuppressProperties state context
        | Action.CompleteProperty -> completeProperty properties state context
        | Action.Cancel
        | Action.Finish -> failwithf $"unreachable action received. {action}"
