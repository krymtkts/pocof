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

    module String =
        let lower (s: string) = s.ToLower()
        let upper (s: string) = s.ToUpper()
        let startsWith (value: string) (s: string) = s.StartsWith(value)
        let split (separator: string) (s: string) = s.Split(separator.ToCharArray())
        let equals (opt: StringComparison) (value: string) (s: string) = s.Equals(value, opt)
        let trim (s: string) = s.Trim()
        let replace (oldValue: string) (newValue: string) (s: string) = s.Replace(oldValue, newValue)

    let swap (l, r) = (r, l)
    let alwaysTrue _ = true

module Data =
    open System
    open System.Collections
    open System.Management.Automation
    open Microsoft.FSharp.Reflection

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type Entry =
        | Obj of PSObject
        | Dict of DictionaryEntry

    let unwrap (entries: Entry list) =
        entries
        |> List.map (function
            | Entry.Dict(dct) -> dct :> obj
            | Entry.Obj(o) -> o)


    let private tryFromStringExcludes<'a> (excludes: Set<string>) s =
        let name = String.lower s
        let aType = typeof<'a>

        match
            FSharpType.GetUnionCases aType
            |> Seq.filter (fun u -> Set.contains u.Name excludes |> not)
            |> Seq.tryFind (fun u -> u.Name |> String.lower = name)
        with
        | Some u -> Ok <| (FSharpValue.MakeUnion(u, [||]) :?> 'a)
        | _ -> Error <| $"Unknown %s{aType.Name} '%s{s}'."

    let private fromString<'a> s =
        let name = String.lower s
        let aType = typeof<'a>

        match
            FSharpType.GetUnionCases aType
            |> Seq.tryFind (fun u -> u.Name |> String.lower = name)
        with
        | Some u -> FSharpValue.MakeUnion(u, [||]) :?> 'a
        | _ -> failwithf $"Unknown %s{aType.Name} '%s{s}'."

    let private toString (x: 'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let (|Prefix|_|) (p: string) (s: string) =
        match String.startsWith p s with
        | true -> Some <| s.Substring(p.Length)
        | _ -> None

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type Action =
        | Noop
        | Cancel
        | Finish
        // move cursor.
        | BackwardChar
        | ForwardChar
        | BeginningOfLine
        | EndOfLine
        // edit query.
        | AddQuery of string
        | DeleteBackwardChar
        | DeleteForwardChar
        | KillBeginningOfLine
        | KillEndOfLine
        // select query.
        | SelectBackwardChar
        | SelectForwardChar
        | SelectToBeginningOfLine
        | SelectToEndOfLine
        | SelectAll
        // toggle options.
        | RotateMatcher
        | RotateOperator
        | ToggleCaseSensitive
        | ToggleInvertFilter
        | ToggleSuppressProperties
        // move line selection.
        | SelectLineUp
        | SelectLineDown
        // scroll page.
        | ScrollPageUp
        | ScrollPageDown
        // property completion.
        | CompleteProperty

    [<RequireQualifiedAccess>]
    module Action =
        let fromString = tryFromStringExcludes<Action> <| set [ "AddQuery" ]

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
        | None

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
        | Search of string
        | Rotate of string * int * string list

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type Refresh =
        | Required
        | NotRequired

    type KeyPattern = { Modifier: int; Key: ConsoleKey }

    type InternalConfig =
        { Layout: Layout
          Keymaps: Map<KeyPattern, Action>
          NotInteractive: bool }

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type InputMode =
        | Input
        | Select of int

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

        let moveCursor (state: QueryState) (step: int) =
            let x =
                match state.Cursor + step with
                | x when x < 0 -> 0
                | x when x > String.length state.Query -> String.length state.Query
                | _ -> state.Cursor + step

            { state with Cursor = x }

        let setCursor (state: QueryState) (x: int) = { state with Cursor = x }

        let setInputMode (mode: InputMode) (state: QueryState) = { state with InputMode = mode }

        let getQuerySelection (cursor: int) (state: QueryState) =
            match state.Cursor, cursor with
            | _, 0 -> state.InputMode
            | x, y when (x + y < 0) || (x + y > String.length state.Query) -> state.InputMode
            | _ ->
                let s =
                    match state.InputMode with
                    | InputMode.Input -> 0
                    | InputMode.Select(s) -> s

                InputMode.Select(s + cursor)

        let backspaceQuery (state: QueryState) (size: int) = // NOTE: size is non-negative.
            let index, count =
                match String.length state.Query, state.Cursor with
                | len, cur when len - cur < 0 ->
                    len,
                    match size + len - cur with
                    | s when s < 0 -> 0
                    | s -> s
                | _, cur -> cur, size
                |> function
                    | cursor, size -> cursor - size, size

            { state with
                Query = state.Query.Remove(index, count)
                Cursor = index }

        let deleteQuery (state: QueryState) (size: int) = // NOTE: size is non-negative.
            match String.length state.Query, state.Cursor with
            | len, cur when len - cur < 0 -> { state with Cursor = len }
            | _ ->
                { state with
                    Query = state.Query.Remove(state.Cursor, size) }

        let deleteSelection (state: QueryState) =
            match state.InputMode with
            | InputMode.Input -> state
            | InputMode.Select c ->
                let si, c =
                    match state.Cursor, state.Cursor - c with
                    | s, e when s < e -> s, e
                    | e, s -> s, e

                { state with
                    Query = state.Query.Remove(si, c - si)
                    Cursor = si
                    InputMode = InputMode.Input }

        let getCurrentProperty (state: QueryState) =
            let s = state.Query.Substring(0, state.Cursor) |> String.split " " |> Seq.last

#if DEBUG
            Logger.LogFile [ $"query '{state.Query}' x '{state.Cursor}' string '{s}'" ]
#endif

            match s with
            | Prefix ":" p -> PropertySearch.Search p
            | _ -> PropertySearch.NoSearch

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
                    | Operator.And -> Operator.None
                    | Operator.None -> Operator.Or }

        let toggleCaseSensitive (condition: QueryCondition) =
            { condition with
                CaseSensitive = not condition.CaseSensitive }

        let toggleInvertFilter (condition: QueryCondition) =
            { condition with
                Invert = not condition.Invert }

    type InternalState =
        { QueryState: QueryState
          QueryCondition: QueryCondition
          PropertySearch: PropertySearch
          Notification: string
          SuppressProperties: bool
          Properties: string list
          Prompt: string
          FilteredCount: int
          ConsoleWidth: int
          Refresh: Refresh }

    module InternalState =
        let private anchor = ">"

        let prompt (state: InternalState) = $"%s{state.Prompt}%s{anchor}"

        let queryInfo (state: InternalState) =
            $" %O{state.QueryCondition} [%d{state.FilteredCount}]"

        let getWindowWidth (state: InternalState) =
            let left = prompt state
            let right = queryInfo state

#if DEBUG
            Logger.LogFile
                [ $"ConsoleWidth '{state.ConsoleWidth}' left '{String.length left}' right '{String.length right}'" ]
#endif

            state.ConsoleWidth - String.length left - String.length right

        let getX (state: InternalState) =
            (prompt state |> String.length) + state.QueryState.Cursor
            - state.QueryState.WindowBeginningCursor

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
            | true -> refresh state
            | _ -> noRefresh state

        let updateWindowWidth (state: InternalState) =
            { state with
                InternalState.QueryState.WindowWidth = getWindowWidth state }

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

        let updateConsoleWidth (consoleWidth: int) (state: InternalState) =
            { state with
                ConsoleWidth = consoleWidth }
            |> updateWindowWidth

        let updateFilteredCount (count: int) (state: InternalState) =
            { state with FilteredCount = count } |> updateWindowWidth

    type Position = { Y: int; Height: int }

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
          Layout: string
          Keymaps: Map<KeyPattern, Action>
          Properties: string list
          EntryCount: int
          ConsoleWidth: int
          ConsoleHeight: int }

    let initConfig (p: IncomingParameters) =
        let qs =
            { Query = p.Query
              Cursor = String.length p.Query
              WindowBeginningCursor = 0 // NOTE: adjust later.
              WindowWidth = p.ConsoleWidth
              InputMode = InputMode.Input }

        let s =
            { QueryState = qs
              QueryCondition =
                { Matcher = Matcher.fromString p.Matcher
                  Operator = Operator.fromString p.Operator
                  CaseSensitive = p.CaseSensitive
                  Invert = p.InvertQuery }
              PropertySearch = QueryState.getCurrentProperty qs
              Notification = ""
              SuppressProperties = p.SuppressProperties
              Properties = p.Properties
              Prompt = p.Prompt
              FilteredCount = p.EntryCount
              ConsoleWidth = p.ConsoleWidth
              Refresh = Refresh.Required }
            |> InternalState.updateWindowWidth

        { Layout = Layout.fromString p.Layout
          Keymaps = p.Keymaps
          NotInteractive = p.NotInteractive },
        s,
        { Y = 0; Height = p.ConsoleHeight }
