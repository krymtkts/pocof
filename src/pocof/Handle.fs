namespace Pocof

open System

open Data
open Query

module Handle =
    type QueryContext = Query.QueryContext

    let private noop (state: InternalState) (context: QueryContext) =
        struct (InternalState.noRefresh state, context)

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
    let private moveCursorForwardWith = moveCursor 1
    let private forwardChar = moveCursorForwardWith InputMode.Input

    [<Literal>]
    let wordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"

    let
#if !DEBUG
        inline
#endif
        private isWordDelimiter
            (wordDelimiters: string)
            (c: char)
            =
        Char.IsWhiteSpace c || wordDelimiters.IndexOf(c) >= 0

    let private measureBackwardWordMove (wordDelimiters: string) (query: string) (cursor: int) =
        if cursor <= 0 || cursor > query.Length then
            0
        else
            let mutable i = cursor

            while i > 0 && isWordDelimiter wordDelimiters query.[i - 1] do
                i <- i - 1

            while i > 0 && not (isWordDelimiter wordDelimiters query.[i - 1]) do
                i <- i - 1

            cursor - i

    let private measureForwardWordMove (wordDelimiters: string) (query: string) (cursor: int) =
        if cursor < 0 || cursor >= query.Length then
            0
        else
            let len = query.Length
            let mutable i = cursor

            while i < len && not (isWordDelimiter wordDelimiters query.[i]) do
                i <- i + 1

            while i < len && isWordDelimiter wordDelimiters query.[i] do
                i <- i + 1

            i - cursor

    let private backwardWord (wordDelimiters: string) (state: InternalState) =
        let i =
            measureBackwardWordMove wordDelimiters state.QueryState.Query state.QueryState.Cursor

        moveCursor -i InputMode.Input state

    let private forwardWord (wordDelimiters: string) (state: InternalState) =
        let i =
            measureForwardWordMove wordDelimiters state.QueryState.Query state.QueryState.Cursor

        moveCursor i InputMode.Input state

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
        let removeQuery, limit =
            match direction with
            | Direction.Backward -> QueryState.backspaceQuery, 0
            | Direction.Forward -> QueryState.deleteQuery, String.length state.QueryState.Query

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
        deleteWord (measureBackwardWordMove wordDelimiters) Direction.Backward
        <| expandSelection max
        <| calculateRemovalSize (-)

    let private deleteForwardWord wordDelimiters =
        deleteWord (measureForwardWordMove wordDelimiters) Direction.Forward
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

        let state, context, beginning =
            match state.QueryState.InputMode with
            | InputMode.Input -> state, context, state.QueryState.Cursor
            | InputMode.Select c ->
                let beginning = min state.QueryState.Cursor <| state.QueryState.Cursor - c
                let struct (state, context) = setCursor beginning InputMode.Input state context
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

    let private selectWord (findCursor: string -> int -> int) operator (converter: int -> int) (state: InternalState) =
        let cursor = findCursor state.QueryState.Query state.QueryState.Cursor

        setCursor
        <| operator state.QueryState.Cursor cursor
        <| QueryState.getQuerySelection (converter cursor) state.QueryState
        <| state

    let private selectBackwardWord wordDelimiters =
        selectWord (measureBackwardWordMove wordDelimiters) (-) (~-)

    let private selectForwardWord wordDelimiters =
        selectWord (measureForwardWordMove wordDelimiters) (+) id

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
        let splitQuery state keyword candidate =
            let basePosition = state.QueryState.Cursor - String.length keyword
            let head = state.QueryState.Query |> String.upToIndex basePosition
            let tail = state.QueryState.Query |> String.fromIndex state.QueryState.Cursor

            match candidate with
            | AlreadyCompleted keyword tail rest -> struct (basePosition, head, rest)
            | _ -> struct (basePosition, head, tail)

        let buildValues head next tail keyword candidates basePosition context =
            let state =
                { state with
                    InternalState.QueryState.Query = $"%s{head}%s{next}%s{tail}"
                    InternalState.QueryState.Cursor = basePosition + String.length next
                    PropertySearch = PropertySearch.Rotate(keyword, candidates) }
                |> InternalState.refresh

            struct (state, context |> QueryContext.prepareQuery state)

        match state.PropertySearch with
        | PropertySearch.NoSearch -> struct (InternalState.noRefresh state, context)
        | PropertySearch.Search keyword ->
            let candidates = properties |> Seq.filter (String.startsWithIgnoreCase keyword)

            match candidates |> Seq.isEmpty with
            | true -> struct (InternalState.noRefresh state, context)
            | _ ->
                let candidate = Seq.head candidates
                let struct (basePosition, head, tail) = splitQuery state keyword candidate
#if DEBUG
                Logger.LogFile [ $"Search keyword '{keyword}' head '{head}' candidate '{candidate}' tail '{tail}'" ]
#endif
                buildValues head candidate tail keyword (candidates |> Seq.cycle) basePosition context
        | PropertySearch.Rotate(keyword, candidates) ->
            let cur = candidates |> Seq.head
            let candidates = candidates |> Seq.tail
            let next = candidates |> Seq.head
            let struct (basePosition, head, tail) = splitQuery state cur next
#if DEBUG
            Logger.LogFile [ $"Rotate keyword '{keyword}' head '{head}' cur '{cur}' next '{next}' tail '{tail}'" ]
#endif
            buildValues head next tail keyword candidates basePosition context

    let invokeAction
        (wordDelimiters: string)
        (properties: string seq)
        (state: InternalState)
        (context: QueryContext)
        (action: Action)
        : struct (InternalState * QueryContext) =
        match action with
        | Action.Noop -> noop state context
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
