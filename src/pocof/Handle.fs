namespace Pocof

open System

open Data
open Query

module Handle =
    type QueryContext = Query.QueryContext

    let private addQuery (state: InternalState) (pos: Position) (context: QueryContext) (query: string) =
        let qs =
            QueryState.deleteSelection state.QueryState
            |> QueryState.addQuery query

        let state =
            state
            |> InternalState.updateQueryState qs
            |> InternalState.refresh
            |> InternalState.prepareNotification

        state, pos, context |> QueryContext.prepareQuery state

    let private updateCursor
        (update: QueryState -> int -> QueryState)
        (cursor: int)
        (mode: InputMode)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =
        let qs =
            update state.QueryState cursor
            |> QueryState.setInputMode mode

        state
        |> InternalState.refreshIfTrue (state.QueryState.Cursor <> qs.Cursor)
        |> InternalState.updateQueryState qs,
        pos,
        context

    let private moveCursor = updateCursor QueryState.moveCursor
    let private moveCursorBackwardWith = moveCursor -1
    let private backwardChar = moveCursorBackwardWith InputMode.Input
    let private moveCursorForwardWith (mode: InputMode) (state: InternalState) = moveCursor 1 mode state
    let private forwardChar = moveCursorForwardWith InputMode.Input

    [<Literal>]
    let wordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"

    let private isWordDelimiter (wordDelimiters: string) (c: char) =
        Char.IsWhiteSpace c
        || wordDelimiters.IndexOf(c) >= 0

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
        isWordDelimiter wordDelimiters
        >> not
        |> findCursorOfChar

    let private findWordCursorWith substring findA findB (wordDelimiters: string) (query: string) (cursor: int) =
        if String.length query < cursor then
            List.Empty, 0
        else
            let str = substring cursor query |> List.ofSeq
            // NOTE: emulate the behavior of the backward-word function in the PSReadLine.
            findA wordDelimiters str 0
            ||> findB wordDelimiters

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
        setCursor
        <| String.length state.QueryState.Query
        <| InputMode.Input
        <| state

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<NoEquality>]
    [<Struct>]
    type private Direction =
        | Backward
        | Forward

    let private removeSelection (state: InternalState) (pos: Position) (context: QueryContext) =
        let qs = QueryState.deleteSelection state.QueryState

        let state =
            state
            |> InternalState.refresh
            |> InternalState.updateQueryState qs
            |> InternalState.prepareNotification

        state, pos, context |> QueryContext.prepareQuery state

    let private removeChars
        (direction: Direction)
        (size: int)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =
        let removeQuery, limit =
            match direction with
            | Direction.Backward -> QueryState.backspaceQuery, 0
            | Direction.Forward -> QueryState.deleteQuery, String.length state.QueryState.Query

        match state.QueryState.Cursor with
        | x when x = limit -> InternalState.noRefresh state, pos, context
        | _ ->
            let qs = removeQuery state.QueryState size

            let state =
                state
                |> InternalState.refreshIfTrue (
                    state.QueryState.Query <> qs.Query
                    || state.QueryState.Cursor <> qs.Cursor
                )
                |> InternalState.updateQueryState qs
                |> InternalState.prepareNotification

            state, pos, context |> QueryContext.prepareQuery state

    let private removeCharsWithInputMode
        (direction: Direction)
        (size: int)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =
        match state.QueryState.InputMode with
        | InputMode.Select _ -> removeSelection state pos context
        | _ -> removeChars direction size state pos context

    let private deleteBackwardChar = removeCharsWithInputMode Direction.Backward 1
    let private deleteForwardChar = removeCharsWithInputMode Direction.Forward 1

    let private expandSelection
        (getSelection: int -> int -> int)
        _ // NOTE: to match the signature of the calculateRemovalSize function.
        (selection: int)
        (wordCursor: int)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =
        let mode =
            getSelection selection wordCursor
            |> InputMode.Select

        let state =
            InternalState.updateQueryState
            <| QueryState.setInputMode mode state.QueryState
            <| state

        removeSelection state pos context

    let private calculateRemovalSize
        (op: int -> int -> int)
        (direction: Direction)
        (selection: int)
        (wordCursor: int)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =
        let cursor = state.QueryState.Cursor - selection
        let state, pos, context = setCursor cursor InputMode.Input state pos context
        removeChars direction (op wordCursor selection) state pos context

    let private deleteWord
        findCursor
        direction
        handleBackwardSelection
        handleForwardSelection
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        =
        let wordCursor = findCursor state.QueryState.Query state.QueryState.Cursor |> snd

        match state.QueryState.InputMode with
        | Input -> removeChars direction wordCursor state pos context
        | SelectBackward selection -> handleBackwardSelection direction selection wordCursor state pos context
        | SelectForward selection -> handleForwardSelection direction selection wordCursor state pos context

    let private deleteBackwardWord wordDelimiters =
        deleteWord (findBackwardWordCursor wordDelimiters) Direction.Backward
        <| expandSelection max
        <| calculateRemovalSize (-)

    let private deleteForwardWord wordDelimiters =
        deleteWord (findForwardWordCursor wordDelimiters) Direction.Forward
        <| calculateRemovalSize (+)
        <| expandSelection (fun x y -> min x -y)

    let private deleteBackwardInput (state: InternalState) (pos: Position) (context: QueryContext) =
        let state, pos, context =
            match state.QueryState.InputMode with
            | InputMode.Input -> (state, pos, context)
            | InputMode.Select c ->
                let selection =
                    max state.QueryState.Cursor
                    <| state.QueryState.Cursor - c

                setCursor selection InputMode.Input state pos context

        removeCharsWithInputMode Direction.Backward state.QueryState.Cursor state pos context

    let private deleteForwardInput (state: InternalState) (pos: Position) (context: QueryContext) =
        let queryLength = String.length state.QueryState.Query

        let state, pos, context, beginning =
            match state.QueryState.InputMode with
            | InputMode.Input -> (state, pos, context, state.QueryState.Cursor)
            | InputMode.Select c ->
                let beginning =
                    min state.QueryState.Cursor
                    <| state.QueryState.Cursor - c

                let state, pos, context = setCursor beginning InputMode.Input state pos context
                state, pos, context, beginning

        removeCharsWithInputMode Direction.Forward (queryLength - beginning) state pos context

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

    let private selectToBeginningOfLine (state: InternalState) (pos: Position) (context: QueryContext) =
        setCursor 0
        <| QueryState.getQuerySelection -state.QueryState.Cursor state.QueryState
        <| state
        <| pos
        <| context

    let private selectToEndOfLine (state: InternalState) (pos: Position) (context: QueryContext) =
        let s =
            String.length state.QueryState.Query
            - state.QueryState.Cursor

        setCursor
        <| String.length state.QueryState.Query
        <| QueryState.getQuerySelection s state.QueryState
        <| state
        <| pos
        <| context

    let private selectAll (state: InternalState) (pos: Position) (context: QueryContext) =
        let s = String.length state.QueryState.Query

        let qs =
            QueryState.setInputMode
            <| InputMode.Select s
            <| QueryState.setCursor state.QueryState s

        state
        |> InternalState.refresh
        |> InternalState.updateQueryState qs,
        pos,
        context

    let private rotateMatcher (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            state
            |> InternalState.rotateMatcher
            |> InternalState.refresh
            |> InternalState.prepareNotification

        state, pos, context |> QueryContext.prepareQuery state

    let private rotateOperator (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            state
            |> InternalState.rotateOperator
            |> InternalState.refresh

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

        state, pos, context |> QueryContext.prepareQuery state

    let private toggleInvertFilter (state: InternalState) (pos: Position) (context: QueryContext) =
        let state =
            state
            |> InternalState.toggleInvertFilter
            |> InternalState.refresh

        state, pos, context |> QueryContext.prepareQuery state

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
        | true ->
            tail
            |> String.fromIndex (String.length tailHead)
            |> Some
        | _ -> None

    let private completeProperty (state: InternalState) (pos: Position) (context: QueryContext) =
        let splitQuery keyword candidate =
            let basePosition = state.QueryState.Cursor - String.length keyword

            let head =
                state.QueryState.Query
                |> String.upToIndex basePosition

            let tail =
                state.QueryState.Query
                |> String.fromIndex state.QueryState.Cursor

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
                |> Seq.filter (String.startsWithIgnoreCase keyword)
                |> List.ofSeq

            match candidates |> Seq.length with
            | 0 -> InternalState.noRefresh state, pos, context
            | _ ->
                let candidate = Seq.head candidates
                let basePosition, head, tail = splitQuery keyword candidate
#if DEBUG
                Logger.LogFile [ $"Search keyword '{keyword}' head '{head}' candidate '{candidate}' tail '{tail}'" ]
#endif
                buildValues head candidate tail keyword 0 candidates basePosition
        | PropertySearch.Rotate (keyword, i, candidates) ->
            let cur = candidates |> Seq.item i
            let i = (i + 1) % Seq.length candidates
            let next = candidates |> Seq.item i
            let basePosition, head, tail = splitQuery cur next
#if DEBUG
            Logger.LogFile [ $"Rotate keyword '{keyword}' head '{head}' cur '{cur}' next '{next}' tail '{tail}'" ]
#endif
            buildValues head next tail keyword i candidates basePosition

    let invokeAction
        (wordDelimiters: string)
        (state: InternalState)
        (pos: Position)
        (context: QueryContext)
        (action: Action)
        =
        match action with
        | Action.Noop -> InternalState.noRefresh state, pos, context
        | Action.AddQuery query -> addQuery state pos context query
        | Action.BackwardChar -> backwardChar state pos context
        | Action.ForwardChar -> forwardChar state pos context
        | Action.BackwardWord -> backwardWord wordDelimiters state pos context
        | Action.ForwardWord -> forwardWord wordDelimiters state pos context
        | Action.BeginningOfLine -> beginningOfLine state pos context
        | Action.EndOfLine -> endOfLine state pos context
        | Action.DeleteBackwardChar -> deleteBackwardChar state pos context
        | Action.DeleteForwardChar -> deleteForwardChar state pos context
        | Action.DeleteBackwardWord -> deleteBackwardWord wordDelimiters state pos context
        | Action.DeleteForwardWord -> deleteForwardWord wordDelimiters state pos context
        | Action.DeleteBackwardInput -> deleteBackwardInput state pos context
        | Action.DeleteForwardInput -> deleteForwardInput state pos context
        | Action.SelectBackwardChar -> selectBackwardChar state pos context
        | Action.SelectForwardChar -> selectForwardChar state pos context
        | Action.SelectBackwardWord -> selectBackwardWord wordDelimiters state pos context
        | Action.SelectForwardWord -> selectForwardWord wordDelimiters state pos context
        | Action.SelectToBeginningOfLine -> selectToBeginningOfLine state pos context
        | Action.SelectToEndOfLine -> selectToEndOfLine state pos context
        | Action.SelectAll -> selectAll state pos context
        | Action.RotateMatcher -> rotateMatcher state pos context
        | Action.RotateOperator -> rotateOperator state pos context
        | Action.ToggleCaseSensitive -> toggleCaseSensitive state pos context
        | Action.ToggleInvertFilter -> toggleInvertFilter state pos context
        | Action.ToggleSuppressProperties -> toggleSuppressProperties state pos context
        | Action.CompleteProperty -> completeProperty state pos context
        | Action.Cancel
        | Action.Finish -> failwithf $"unreachable action received. {action}"
