namespace Pocof

#if DEBUG

[<AutoOpen>]
module Debug =
    open System
    open System.IO
    open System.Runtime.CompilerServices
    open System.Runtime.InteropServices

    let lockObj = new obj ()

    let logPath = "./debug.log"

    [<AbstractClass; Sealed>]
    type Logger =
        static member LogFile
            (
                res,
                [<Optional; DefaultParameterValue(""); CallerMemberName>] caller: string,
                [<CallerFilePath; Optional; DefaultParameterValue("")>] path: string,
                [<CallerLineNumber; Optional; DefaultParameterValue(0)>] line: int
            ) =

            // NOTE: lock to avoid another process error when dotnet test.
            lock lockObj (fun () ->
                use sw = new StreamWriter(logPath, true)

                res
                |> List.iter (
                    fprintfn
                        sw
                        "[%s] %s at %d %s <%A>"
                        (DateTimeOffset.Now.ToString("yyyy-MM-dd'T'HH:mm:ss.fffzzz"))
                        path
                        line
                        caller
                ))
#endif

[<AutoOpen>]
module LanguageExtension =
    open System
    open System.Linq

    module Option =
        let dispose (d: 'a option when 'a :> IDisposable) = d |> Option.iter _.Dispose()

    module Regex =
        open System.Text.RegularExpressions
        let split (separator: string) (s: string) = Regex.Split(s, separator)

    module String =
        let lower (s: string) = s.ToLower()
        let upper (s: string) = s.ToUpper()
        let startsWith (value: string) (s: string) = s.StartsWith(value)

        let startsWithIgnoreCase (value: string) (s: string) =
            s.StartsWith(value, StringComparison.OrdinalIgnoreCase)

        let split (separator: string) (s: string) = s.Split(separator.ToCharArray())

        let split2 (separators: string array) (s: string) =
            s.Split(separators, StringSplitOptions.None)

        let equals (opt: StringComparison) (value: string) (s: string) = s.Equals(value, opt)
        let trim (s: string) = s.Trim()
        let replace (oldValue: string) (newValue: string) (s: string) = s.Replace(oldValue, newValue)
        let fromIndex (index: int) (s: string) = s.Substring(index)
        let upToIndex (index: int) (s: string) = s.Substring(0, index)

    let
#if !DEBUG
        inline
#endif
        alwaysTrue
            _
            =
        true

    let (|Ascending|) (x, y) = if x < y then (x, y) else (y, x)

    let (|Negative|_|) (value: int) = value < 0

    let (|Natural|_|) (value: int) =
        match value with
        | x when x > 0 -> Some x
        | _ -> None

    // NOTE: Follow the naming style of built-in types.
    // fsharplint:disable-next-line
    type pseq<'T> = ParallelQuery<'T>

    module PSeq =
        let ofSeq (source: seq<'T>) : pseq<'T> = source.AsParallel().AsOrdered()

        let filter predicate (source: pseq<'T>) : pseq<'T> =
            ParallelEnumerable.Where(source, Func<_, _>(predicate))

        let length (source: pseq<'T>) : int = ParallelEnumerable.Count(source)

        let empty<'T> : pseq<'T> = ParallelEnumerable.Empty<'T>()

    type Collections.DictionaryEntry with
        member __.Item
            with get (prop: string) =
                prop
                |> function
                    | "Key" -> __.Key
                    | "Value" -> __.Value
                    | _ -> null

    type Management.Automation.PSObject with
        member __.Item
            with get (prop: string) =
                // NOTE: should check empty prop by the caller.
                __.Properties[prop]
                |> function
                    | null -> null
                    | p -> p.Value

module Data =
    open System
    open System.Collections
    open System.Management.Automation
    open Microsoft.FSharp.Reflection

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type Entry =
        | Obj of o: PSObject
        | Dict of d: DictionaryEntry

        member __.Item
            with get (prop: string) =
                __
                |> function
                    | Obj(o) -> o[prop]
                    | Dict(d) -> d[prop]

    let unwrap (entries: Entry seq) =
        entries
        |> Seq.map (function
            | Entry.Dict(dct) -> dct :> obj
            | Entry.Obj(o) -> o)

    let (|Found|_|) aType excludes name =
        FSharpType.GetUnionCases aType
        |> Seq.filter (fun u -> Set.contains u.Name excludes |> not)
        |> Seq.tryFind (fun u -> u.Name |> String.lower = name)

    let private tryFromStringExcludes<'a> (excludes: Set<string>) s =
        let name = String.lower s
        let aType = typeof<'a>

        match name with
        | Found aType excludes u -> Ok <| (FSharpValue.MakeUnion(u, [||]) :?> 'a)
        | _ -> Error <| $"Unknown %s{aType.Name} '%s{s}'."

    let private fromString<'a> s =
        tryFromStringExcludes<'a> (set []) s
        |> function
            | Ok x -> x
            | Error e -> failwith e

    let private toString (x: 'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let (|Prefix|_|) (p: string) (s: string) =
        match String.startsWith p s with
        | true -> s |> String.fromIndex (String.length p) |> Some
        | _ -> None

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<Struct>]
    type Action =
        | Noop
        | Cancel
        | Finish
        // move cursor.
        | BackwardChar
        | ForwardChar
        | BackwardWord
        | ForwardWord
        | BeginningOfLine
        | EndOfLine
        // edit query.
        | AddQuery of query: string
        | DeleteBackwardChar
        | DeleteForwardChar
        | DeleteBackwardWord
        | DeleteForwardWord
        | DeleteBackwardInput
        | DeleteForwardInput
        // select query.
        | SelectBackwardChar
        | SelectForwardChar
        | SelectBackwardWord
        | SelectForwardWord
        | SelectToBeginningOfLine
        | SelectToEndOfLine
        | SelectAll
        // toggle options.
        | RotateMatcher
        | RotateOperator
        | ToggleCaseSensitive
        | ToggleInvertFilter
        | ToggleSuppressProperties
        // property completion.
        | CompleteProperty

    [<RequireQualifiedAccess>]
    module Action =
        let fromString = tryFromStringExcludes<Action> <| set [ nameof Action.AddQuery ]

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type Matcher =
        | Eq
        | Like
        | Match

        override __.ToString() = toString __ |> String.lower

    [<RequireQualifiedAccess>]
    module Matcher =
        let fromString = fromString<Matcher>

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type Operator =
        | And
        | Or

        override __.ToString() = toString __ |> String.lower

    [<RequireQualifiedAccess>]
    module Operator =
        let fromString = fromString<Operator>

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type Layout =
        | TopDown
        | TopDownHalf
        | BottomUp
        | BottomUpHalf

    [<RequireQualifiedAccess>]
    module Layout =
        let fromString = fromString<Layout>

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type PropertySearch =
        | NoSearch
        | Search of keyword: string
        | Rotate of keyword: string * index: int * candidates: string seq

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type Refresh =
        | Required
        | NotRequired

    // NOTE: Comparison and Equality are required.
    type KeyPattern = { Modifier: int; Key: ConsoleKey }

    [<NoComparison>]
    type InternalConfig =
        { Layout: Layout
          Keymaps: Map<KeyPattern, Action>
          NotInteractive: bool
          WordDelimiters: string
          Prompt: string
          PromptLength: int }

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type InputMode =
        | Input
        // Note: positive number is the backward selection from the cursor. negative number is the forward selection from the cursor.
        | Select of count: int

    let (|Input|SelectForward|SelectBackward|) (x: InputMode) =
        match x with
        | InputMode.Input -> Input
        | InputMode.Select x when x > 0 -> SelectBackward x
        | InputMode.Select x -> SelectForward x

    [<NoComparison>]
    type QueryState =
        { Query: string
          Cursor: int
          WindowBeginningCursor: int
          WindowWidth: int
          InputMode: InputMode }

    module QueryState =
        let addQuery (query: string) (state: QueryState) =
            { state with
                Query = state.Query.Insert(state.Cursor, query)
                Cursor = state.Cursor + String.length query }

        let (|OverQuery|_|) (query: string) (cursor: int) =
            let ql = String.length query

            match cursor with
            | x when x > ql -> Some ql
            | _ -> None

        let moveCursor (state: QueryState) (step: int) =
            let x =
                match state.Cursor + step with
                | Negative -> 0
                | OverQuery state.Query c -> c
                | c -> c

            { state with Cursor = x }

        let setCursor (state: QueryState) (x: int) = { state with Cursor = x }

        let setInputMode (mode: InputMode) (state: QueryState) = { state with InputMode = mode }

        let getQuerySelection (cursor: int) (state: QueryState) =
            match state.Cursor, cursor with
            | _, 0 -> state.InputMode
            | x, y when (x + y < 0) || (x + y > String.length state.Query) -> state.InputMode
            | _ ->
                match state.InputMode with
                | InputMode.Input -> 0
                | InputMode.Select(s) -> s
                |> (+) cursor
                |> function
                    | 0 -> InputMode.Input
                    | s -> s |> InputMode.Select

        let backspaceQuery (state: QueryState) (size: int) = // NOTE: size is non-negative.
            let index, count =
                let ql = String.length state.Query

                match ql - state.Cursor with
                | Negative ->
                    ql,
                    match size + ql - state.Cursor with
                    | Negative -> 0
                    | s -> s
                | _ -> state.Cursor, size
                |> function
                    | cursor, size -> cursor - size, size

            { state with
                Query = state.Query.Remove(index, count)
                Cursor = index }

        let deleteQuery (state: QueryState) (size: int) = // NOTE: size is non-negative.
            let ql = String.length state.Query

            match ql - state.Cursor with
            | Negative -> { state with Cursor = ql }
            | _ ->
                { state with
                    Query = state.Query.Remove(state.Cursor, size) }

        let deleteSelection (state: QueryState) =
            match state.InputMode with
            | InputMode.Input -> state
            | InputMode.Select c ->
                let si, c =
                    match state.Cursor, state.Cursor - c with
                    | Ascending x -> x

                { state with
                    Query = state.Query.Remove(si, c - si)
                    Cursor = si
                    InputMode = InputMode.Input }

        let getCurrentProperty (state: QueryState) =
            let s = state.Query |> String.upToIndex state.Cursor |> String.split " " |> Seq.last

#if DEBUG
            Logger.LogFile [ $"Query '{state.Query}' Cursor '{state.Cursor}' string '{s}'" ]
#endif

            match s with
            | Prefix ":" p -> PropertySearch.Search p
            | _ -> PropertySearch.NoSearch

    [<NoComparison>]
    type QueryCondition =
        { Matcher: Matcher
          Operator: Operator
          CaseSensitive: bool
          Invert: bool }

        override __.ToString() =
            [ match __.CaseSensitive with
              | true -> "c"
              | _ -> ""
              // NOTE: use ToString to avoid extra branches when calculating coverages.
              match __.Matcher, __.Invert with
              | Matcher.Eq, true -> "ne"
              | m, true -> "not" + m.ToString()
              | m, _ -> m.ToString()
              " "
              __.Operator.ToString() ]
            |> String.concat ""

    [<NoComparison>]
    module QueryCondition =
        let rotateMatcher (condition: QueryCondition) =
            { condition with
                Matcher =
                    match condition.Matcher with
                    | Matcher.Eq -> Matcher.Like
                    | Matcher.Like -> Matcher.Match
                    | Matcher.Match -> Matcher.Eq }

        let rotateOperator (condition: QueryCondition) =
            { condition with
                Operator =
                    match condition.Operator with
                    | Operator.Or -> Operator.And
                    | Operator.And -> Operator.Or }

        let toggleCaseSensitive (condition: QueryCondition) =
            { condition with
                CaseSensitive = not condition.CaseSensitive }

        let toggleInvertFilter (condition: QueryCondition) =
            { condition with
                Invert = not condition.Invert }

    [<NoComparison>]
    type InternalState =
        { QueryState: QueryState
          QueryCondition: QueryCondition
          PropertySearch: PropertySearch
          Notification: string option
          SuppressProperties: bool
          Properties: Generic.IReadOnlyCollection<string>
          PropertyMap: Generic.IReadOnlyDictionary<string, string>
          Refresh: Refresh }

    module InternalState =
        let queryInfo (state: InternalState) (count: int) =
            $" %O{state.QueryCondition} [%d{count}]"

        let getX promptLength (state: InternalState) =
            promptLength + state.QueryState.Cursor - state.QueryState.WindowBeginningCursor

        let updateQueryState (qs: QueryState) (state: InternalState) =
            { state with
                QueryState = qs
                PropertySearch = QueryState.getCurrentProperty qs }

        let refresh (state: InternalState) =
            { state with
                Refresh = Refresh.Required }

        let noRefresh (state: InternalState) =
            { state with
                Refresh = Refresh.NotRequired }

        let refreshIfTrue (b: bool) (state: InternalState) =
            match b with
            | true -> refresh
            | _ -> noRefresh
            <| state

        let rotateMatcher (state: InternalState) =
            { state with
                QueryCondition = state.QueryCondition |> QueryCondition.rotateMatcher }

        let rotateOperator (state: InternalState) =
            { state with
                QueryCondition = state.QueryCondition |> QueryCondition.rotateOperator }

        let toggleCaseSensitive (state: InternalState) =
            { state with
                QueryCondition = state.QueryCondition |> QueryCondition.toggleCaseSensitive }

        let toggleInvertFilter (state: InternalState) =
            { state with
                QueryCondition = state.QueryCondition |> QueryCondition.toggleInvertFilter }

        let toggleSuppressProperties (state: InternalState) =
            { state with
                SuppressProperties = not state.SuppressProperties }

        let updateConsoleWidth (promptLength: int) (consoleWidth: int) (state: InternalState) =
            { state with
                InternalState.QueryState.WindowWidth = promptLength |> (+) 1 |> (-) consoleWidth }

        let create
            (queryState: QueryState)
            (queryCondition: QueryCondition)
            (suppressProperties: bool)
            (properties: Generic.IReadOnlyCollection<string>)
            (propertyMap: Generic.IReadOnlyDictionary<string, string>)
            (prompt: int)
            (consoleWidth: int)
            =

            { QueryState = queryState
              QueryCondition = queryCondition
              PropertySearch = QueryState.getCurrentProperty queryState
              Notification = None
              SuppressProperties = suppressProperties
              Properties = properties
              PropertyMap = propertyMap
              Refresh = Refresh.Required }
            |> updateConsoleWidth prompt consoleWidth

    [<NoComparison>]
    [<NoEquality>]
    type IncomingParameters =
        { Query: string
          Matcher: string
          Operator: string
          CaseSensitive: bool
          InvertQuery: bool
          NotInteractive: bool
          SuppressProperties: bool
          Prompt: string
          WordDelimiters: string
          Layout: string
          Keymaps: Map<KeyPattern, Action>
          Properties: Generic.IReadOnlyCollection<string>
          PropertiesMap: Generic.IReadOnlyDictionary<string, string>
          ConsoleWidth: int }

    [<Literal>]
    let private anchor = ">"

    let initConfig (p: IncomingParameters) =
        let prompt = p.Prompt + anchor
        let promptLength = prompt |> String.length

        { Layout = Layout.fromString p.Layout
          Keymaps = p.Keymaps
          NotInteractive = p.NotInteractive
          WordDelimiters = p.WordDelimiters
          Prompt = prompt
          PromptLength = promptLength },
        InternalState.create
            { Query = p.Query
              Cursor = String.length p.Query
              WindowBeginningCursor = 0 // NOTE: adjust later.
              WindowWidth = 0 // NOTE: adjust later.
              InputMode = InputMode.Input }
            { Matcher = Matcher.fromString p.Matcher
              Operator = Operator.fromString p.Operator
              CaseSensitive = p.CaseSensitive
              Invert = p.InvertQuery }
            p.SuppressProperties
            p.Properties
            p.PropertiesMap
            promptLength
            p.ConsoleWidth
