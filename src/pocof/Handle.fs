namespace Pocof

open System

open Data
open Query

module Handle =
    type QueryContext = Query.QueryContext

    let private addQuery (state: InternalState) (context: QueryContext) (query: string) =
        let qs = QueryState.deleteSelection state.QueryState |> QueryState.addQuery query

        let state = state |> InternalState.updateQueryState qs |> InternalState.refresh

        struct (state, context |> QueryContext.prepareQuery state)

    let private updateCursor
        (update: QueryState -> int -> QueryState)
        (cursor: int)
        (mode: InputMode)
        (state: InternalState)
        (context: QueryContext)
        =
        let qs = update state.QueryState cursor |> QueryState.setInputMode mode

        struct (state
                |> InternalState.refreshIfTrue (state.QueryState.Cursor <> qs.Cursor)
                |> InternalState.updateQueryState qs,
                context)

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
    let rec private findCursorOfChar (predicate: char -> bool) (str: char array) (cursor: int) =
        if Array.length str <= cursor then
            struct (Array.Empty(), cursor)
        else
            let c = str.[cursor]

            if predicate c then
                findCursorOfChar predicate str (cursor + 1)
            else
                struct (str, cursor)

    let private findWordCursor (wordDelimiters: string) =
        isWordDelimiter wordDelimiters |> findCursorOfChar

    let rec private findWordDelimiterCursor (wordDelimiters: string) =
        isWordDelimiter wordDelimiters >> not |> findCursorOfChar

    let private findWordCursorWith
        (substring: int -> string -> char array)
        (findA: string -> char array -> int -> struct (char array * int))
        (findB: string -> char array -> int -> struct (char array * int))
        (wordDelimiters: string)
        (query: string)
        (cursor: int)
        =
        if String.length query < cursor then
            0
        else
            let str = substring cursor query
            // NOTE: emulate the behavior of the backward-word function in the PSReadLine.
            findA wordDelimiters str 0 ||*> findB wordDelimiters |> snd'

    let private findBackwardWordCursor =
        findWordCursorWith
            (fun i s -> s |> String.upToIndex i |> _.ToCharArray() |> Array.rev)
            findWordCursor
            findWordDelimiterCursor

    let private findForwardWordCursor =
        findWordCursorWith
            (fun i s -> s |> String.fromIndex i |> _.ToCharArray())
            findWordDelimiterCursor
            findWordCursor

    let private wordAction (findWordCursor: string -> int -> int) (converter: int -> int) (state: InternalState) =
        let i = findWordCursor state.QueryState.Query state.QueryState.Cursor |> converter
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

        struct (state, context |> QueryContext.prepareQuery state)

    let private removeChars (direction: Direction) (size: int) (state: InternalState) (context: QueryContext) =
        let struct (removeQuery, limit) =
            match direction with
            | Direction.Backward -> struct (QueryState.backspaceQuery, 0)
            | Direction.Forward -> struct (QueryState.deleteQuery, String.length state.QueryState.Query)

        match state.QueryState.Cursor with
        | x when x = limit -> struct (InternalState.noRefresh state, context)
        | _ ->
            let qs = removeQuery state.QueryState size

            let state =
                state
                |> InternalState.refreshIfTrue (
                    state.QueryState.Query <> qs.Query || state.QueryState.Cursor <> qs.Cursor
                )
                |> InternalState.updateQueryState qs

            struct (state, context |> QueryContext.prepareQuery state)

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

        setCursor cursor InputMode.Input state context
        ||*> removeChars direction (op wordCursor selection)

    let private deleteWord
        (findCursor: string -> int -> int)
        (direction: Direction)
        handleBackwardSelection
        handleForwardSelection
        (state: InternalState)
        (context: QueryContext)
        =
        let wordCursor = findCursor state.QueryState.Query state.QueryState.Cursor

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
        let struct (state, context) =
            match state.QueryState.InputMode with
            | InputMode.Input -> struct (state, context)
            | InputMode.Select c ->
                let selection = max state.QueryState.Cursor <| state.QueryState.Cursor - c

                setCursor selection InputMode.Input state context

        removeCharsWithInputMode Direction.Backward state.QueryState.Cursor state context

    let private deleteForwardInput (state: InternalState) (context: QueryContext) =
        let queryLength = String.length state.QueryState.Query

        let struct (state, context, beginning) =
            match state.QueryState.InputMode with
            | InputMode.Input -> struct (state, context, state.QueryState.Cursor)
            | InputMode.Select c ->
                let beginning = min state.QueryState.Cursor <| state.QueryState.Cursor - c

                let struct (state, context) = setCursor beginning InputMode.Input state context
                struct (state, context, beginning)

        removeCharsWithInputMode Direction.Forward (queryLength - beginning) state context

    let private selectBackwardChar (state: InternalState) =
        moveCursorBackwardWith
        <| QueryState.getQuerySelection -1 state.QueryState
        <| state

    let private selectForwardChar (state: InternalState) =
        moveCursorForwardWith
        <| QueryState.getQuerySelection 1 state.QueryState
        <| state

    let private selectWord (findCursor: string -> int -> int) operator (converter: int -> int) (state: InternalState) =
        let cursor = findCursor state.QueryState.Query state.QueryState.Cursor

        setCursor
        <| operator state.QueryState.Cursor cursor
        <| QueryState.getQuerySelection (converter cursor) state.QueryState
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

        struct (state |> InternalState.refresh |> InternalState.updateQueryState qs, context)

    let private rotateMatcher (state: InternalState) (context: QueryContext) =
        let state = state |> InternalState.rotateMatcher |> InternalState.refresh

        struct (state, context |> QueryContext.prepareQuery state)

    let private rotateOperator (state: InternalState) (context: QueryContext) =
        let state = state |> InternalState.rotateOperator |> InternalState.refresh

        struct (state, context |> QueryContext.prepareQuery state |> QueryContext.prepareTest state)

    let private toggleCaseSensitive (state: InternalState) (context: QueryContext) =
        let state = state |> InternalState.toggleCaseSensitive |> InternalState.refresh

        struct (state, context |> QueryContext.prepareQuery state)

    let private toggleInvertFilter (state: InternalState) (context: QueryContext) =
        let state = state |> InternalState.toggleInvertFilter |> InternalState.refresh

        struct (state, context |> QueryContext.prepareQuery state)

    let private toggleSuppressProperties (state: InternalState) (context: QueryContext) =
        let state = state |> InternalState.toggleSuppressProperties |> InternalState.refresh

        struct (state, context)

    [<return: Struct>]
    let private (|AlreadyCompleted|_|) (keyword: string) (tail: string) (candidate: string) =
        let rest: string =
            match String.length keyword with
            | 0 -> candidate
            | _ -> candidate |> String.replace keyword ""

        let tailHead =
            let i = tail.IndexOf " "
            if i = -1 then tail else String.upToIndex i tail

        match rest = tailHead with
        | true -> tail |> String.fromIndex (String.length tailHead) |> ValueSome
        | _ -> ValueNone

    let private completeProperty (properties: string seq) (state: InternalState) (context: QueryContext) =
        let splitQuery keyword candidate =
            let basePosition = state.QueryState.Cursor - String.length keyword
            let head = state.QueryState.Query |> String.upToIndex basePosition
            let tail = state.QueryState.Query |> String.fromIndex state.QueryState.Cursor

            match candidate with
            | AlreadyCompleted keyword tail rest -> struct (basePosition, head, rest)
            | _ -> struct (basePosition, head, tail)

        let buildValues head next tail keyword candidates basePosition =
            let state =
                { state with
                    InternalState.QueryState.Query = $"%s{head}%s{next}%s{tail}"
                    InternalState.QueryState.Cursor = basePosition + String.length next
                    PropertySearch = PropertySearch.Rotate(keyword, candidates) }
                |> InternalState.refresh

            struct (state, context |> QueryContext.prepareQuery state) // TODO: Remove the value-type (struct) capture caused by this closure.

        match state.PropertySearch with
        | PropertySearch.NoSearch -> struct (InternalState.noRefresh state, context)
        | PropertySearch.Search keyword ->
            let candidates = properties |> Seq.filter (String.startsWithIgnoreCase keyword)

            match candidates |> Seq.isEmpty with
            | true -> struct (InternalState.noRefresh state, context)
            | _ ->
                let candidate = Seq.head candidates
                let struct (basePosition, head, tail) = splitQuery keyword candidate
#if DEBUG
                Logger.LogFile [ $"Search keyword '{keyword}' head '{head}' candidate '{candidate}' tail '{tail}'" ]
#endif
                buildValues head candidate tail keyword (candidates |> Seq.cycle) basePosition
        | PropertySearch.Rotate(keyword, candidates) ->
            let cur = candidates |> Seq.head
            let candidates = candidates |> Seq.tail
            let next = candidates |> Seq.head
            let struct (basePosition, head, tail) = splitQuery cur next
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
        : struct (InternalState * QueryContext) =
        match action with
        | Action.Noop -> struct (InternalState.noRefresh state, context)
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
