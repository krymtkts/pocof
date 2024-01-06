namespace pocof

#if DEBUG

[<AutoOpen>]
module PocofDebug =
    open System
    open System.IO
    open System.Runtime.CompilerServices
    open System.Runtime.InteropServices

    let lockObj = new obj ()

    let logPath = "./debug.log"

    [<AbstractClass; Sealed>]
    type Logger =
        static member logFile
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

    type String with
        static member lower(s: string) = s.ToLower()
        static member upper(s: string) = s.ToUpper()
        static member startsWith (value: string) (s: string) = s.StartsWith(value)
        static member split (separator: string) (s: string) = s.Split(separator.ToCharArray())
        static member equals (opt: StringComparison) (value: string) (s: string) = s.Equals(value, opt)
        static member trim(s: string) = s.Trim()
        static member replace (oldValue: string) (newValue: string) (s: string) = s.Replace(oldValue, newValue)
        static member padRight (totalWidth: int) (s: string) = s.PadRight(totalWidth)

    let swap (l, r) = (r, l)
    let alwaysTrue _ = true

module PocofData =
    open System
    open System.Management.Automation
    open System.Collections
    open Microsoft.FSharp.Reflection

    type Entry =
        | Obj of PSObject
        | Dict of DictionaryEntry

    let unwrap (entries: Entry list) =
        entries
        |> List.map (function
            | Dict (dct) -> dct :> obj
            | Obj (o) -> o)


    let private tryFromStringExcludes<'a> (excludes: Set<string>) s =
        let name = String.lower s
        let aType = typeof<'a>

        match FSharpType.GetUnionCases aType
              |> Seq.filter (fun u -> Set.contains u.Name excludes |> not)
              |> Seq.tryFind (fun u -> u.Name |> String.lower = name)
            with
        | Some u -> Ok <| (FSharpValue.MakeUnion(u, [||]) :?> 'a)
        | _ -> Error <| $"Unknown %s{aType.Name} '%s{s}'."

    let private fromString<'a> s =
        let name = String.lower s
        let aType = typeof<'a>

        match FSharpType.GetUnionCases aType
              |> Seq.tryFind (fun u -> u.Name |> String.lower = name)
            with
        | Some u -> FSharpValue.MakeUnion(u, [||]) :?> 'a
        | _ -> failwithf $"Unknown %s{aType.Name} '%s{s}'."

    let private toString (x: 'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let (|Prefix|_|) (p: string) (s: string) =
        match String.startsWith p s with
        | true -> Some s.[1..]
        | _ -> None

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

    module Action =
        let fromString =
            tryFromStringExcludes<Action>
            <| set [ "AddQuery" ]

    type Matcher =
        | EQ
        | LIKE
        | MATCH
        override __.ToString() = toString __ |> String.lower

    module Matcher =
        let fromString = fromString<Matcher>

    type Operator =
        | AND
        | OR
        | NONE
        override __.ToString() = toString __ |> String.lower

    module Operator =
        let fromString = fromString<Operator>

    type Layout =
        | TopDown
        | BottomUp

    module Layout =
        let fromString = fromString<Layout>

    type PropertySearch =
        | NoSearch
        | Search of string
        | Rotate of string * int * string list

    type Refresh =
        | Required
        | NotRequired

    type KeyPattern = { Modifier: int; Key: ConsoleKey }

    type InternalConfig =
        { Layout: Layout
          Keymaps: Map<KeyPattern, Action>
          NotInteractive: bool }

    type QueryState =
        { Query: string
          Cursor: int
          WindowBeginningX: int
          WindowWidth: int }

    module QueryState =
        let adjustCursor (state: QueryState) =
            let wx =
                match state.Cursor - state.WindowBeginningX with
                | bx when bx < 0 -> state.WindowBeginningX + bx
                | bx when bx > state.WindowWidth -> state.Cursor - state.WindowWidth
                | _ -> state.WindowBeginningX

#if DEBUG
            Logger.logFile [ $"wx '{wx}' Cursor '{state.Cursor}' WindowBeginningX '{state.WindowBeginningX}' WindowWidth '{state.WindowWidth}'" ]
#endif
            { state with WindowBeginningX = wx }

        let addQuery (state: QueryState) (query: string) =
            { state with
                Query = state.Query.Insert(state.Cursor, query)
                Cursor = state.Cursor + String.length query }
            |> adjustCursor

        let moveCursor (state: QueryState) (step: int) =
            let x =
                match state.Cursor + step with
                | x when x < 0 -> 0
                | x when x > String.length state.Query -> String.length state.Query
                | _ -> state.Cursor + step

            { state with Cursor = x } |> adjustCursor

        let setCursor (state: QueryState) (x: int) =
            { state with Cursor = x } |> adjustCursor

        let backspaceQuery (state: QueryState) (size: int) =
            let cursor, size =
                match String.length state.Query - state.Cursor with
                | x when x < 0 ->
                    String.length state.Query,
                    match size + x with
                    | s when s < 0 -> 0
                    | s -> s
                | _ -> state.Cursor, size

            let i, c =
                match cursor - size with
                | x when x < 0 -> 0, state.Cursor
                | x -> x, size

            { state with
                Query = state.Query.Remove(i, c)
                Cursor = i }
            |> adjustCursor

        let deleteQuery (state: QueryState) (size: int) =
            match String.length state.Query - state.Cursor with
            | x when x < 0 -> { state with Cursor = String.length state.Query }
            | _ ->
                { state with Query = state.Query.Remove(state.Cursor, size) }
                |> adjustCursor

        let getCurrentProperty (state: QueryState) =
            let s =
                state.Query.[.. state.Cursor - 1]
                |> String.split " "
                |> Seq.last

#if DEBUG
            Logger.logFile [ $"query '{state.Query}' x '{state.Cursor}' string '{s}'" ]
#endif

            match s with
            | Prefix ":" p -> Search p
            | _ -> NoSearch

    type QueryCondition =
        { Matcher: Matcher
          Operator: Operator
          CaseSensitive: bool
          Invert: bool }
        override __.ToString() =
            List.append
            <| match __.Matcher, __.CaseSensitive, __.Invert with
               | m, false, false -> [ string m ]
               | m, true, false -> [ "c"; string m ]
               | EQ, false, true -> [ "ne" ]
               | EQ, true, true -> [ "cne" ]
               | m, false, true -> [ "not"; string m ]
               | m, true, true -> [ "notc"; string m ]
            <| [ " "; string __.Operator ]
            |> String.concat ""

    module QueryCondition =
        let rotateMatcher (condition: QueryCondition) =
            { condition with
                Matcher =
                    match condition.Matcher with
                    | EQ -> LIKE
                    | LIKE -> MATCH
                    | MATCH -> EQ }

        let rotateOperator (condition: QueryCondition) =
            { condition with
                Operator =
                    match condition.Operator with
                    | OR -> AND
                    | AND -> NONE
                    | NONE -> OR }

        let toggleCaseSensitive (condition: QueryCondition) =
            { condition with CaseSensitive = not condition.CaseSensitive }

        let toggleInvertFilter (condition: QueryCondition) =
            { condition with Invert = not condition.Invert }

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

        let private prompt (state: InternalState) = $"%s{state.Prompt}%s{anchor}"

        let private queryInfo (state: InternalState) =
            $" %O{state.QueryCondition} [%d{state.FilteredCount}]"

        let getWindowWidth (state: InternalState) =
            let left = prompt state
            let right = queryInfo state

#if DEBUG
            Logger.logFile [ $"ConsoleWidth '{state.ConsoleWidth}' left '{String.length left}' right '{String.length right}'" ]
#endif

            state.ConsoleWidth
            - String.length left
            - String.length right

        let info (state: InternalState) =
            let left = prompt state
            let right = queryInfo state
            let l = getWindowWidth state

            let q =
                // TODO: should use RawUI.LengthInBufferCells instead of String.length for supporting full-width characters.
                match String.length state.QueryState.Query with
                | ql when ql < l -> ql
                | _ -> state.QueryState.WindowBeginningX + l
                |> fun ql -> state.QueryState.Query.[state.QueryState.WindowBeginningX .. ql - 1]
                |> String.padRight l

#if DEBUG
            Logger.logFile [ $"q '{String.length q}' WindowBeginningX '{state.QueryState.WindowBeginningX}' WindowWidth '{l}' ConsoleWidth '{state.ConsoleWidth}'" ]
#endif

            left + q + right

        let getX (state: InternalState) =
            (prompt state |> String.length)
            + state.QueryState.Cursor
            - state.QueryState.WindowBeginningX

        let updateQueryState (qs: QueryState) (state: InternalState) =
            { state with
                QueryState = qs
                PropertySearch = QueryState.getCurrentProperty qs }

        let refresh (state: InternalState) = { state with Refresh = Required }

        let noRefresh (state: InternalState) = { state with Refresh = NotRequired }

        let refreshIfTrue (b: bool) (state: InternalState) =
            match b with
            | true -> refresh state
            | _ -> noRefresh state

        let adjustCursor (state: InternalState) =
            { state with QueryState = QueryState.adjustCursor state.QueryState }

        let updateWindowWidth (state: InternalState) =
            { state with InternalState.QueryState.WindowWidth = getWindowWidth state }
            |> adjustCursor

        let rotateMatcher (state: InternalState) =
            { state with
                QueryCondition =
                    state.QueryCondition
                    |> QueryCondition.rotateMatcher }

        let rotateOperator (state: InternalState) =
            { state with
                QueryCondition =
                    state.QueryCondition
                    |> QueryCondition.rotateOperator }

        let toggleCaseSensitive (state: InternalState) =
            { state with
                QueryCondition =
                    state.QueryCondition
                    |> QueryCondition.toggleCaseSensitive }

        let toggleInvertFilter (state: InternalState) =
            { state with
                QueryCondition =
                    state.QueryCondition
                    |> QueryCondition.toggleInvertFilter }

        let toggleSuppressProperties (state: InternalState) =
            { state with SuppressProperties = not state.SuppressProperties }

        let updateConsoleWidth (consoleWidth: int) (state: InternalState) =
            { state with ConsoleWidth = consoleWidth }
            |> updateWindowWidth

        let updateFilteredCount (count: int) (state: InternalState) =
            { state with FilteredCount = count }
            |> updateWindowWidth

    type Position = { Y: int; Height: int }

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
              WindowBeginningX = 0 // NOTE: adjust later.
              WindowWidth = p.ConsoleWidth }

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
              Refresh = Required }
            |> InternalState.updateWindowWidth

        { Layout = Layout.fromString p.Layout
          Keymaps = p.Keymaps
          NotInteractive = p.NotInteractive },
        s,
        { Y = 0; Height = p.ConsoleHeight }
