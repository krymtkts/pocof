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
        let startsWith (value: string) (s: string) = s.StartsWith(value)

        let startsWithIgnoreCase (value: string) (s: string) =
            s.StartsWith(value, StringComparison.OrdinalIgnoreCase)

        let split (separator: string) (s: string) = s.Split(separator.ToCharArray())

        let split2 (separators: string array) (s: string) =
            s.Split(separators, StringSplitOptions.None)

        let equals (opt: StringComparison) (value: string) (s: string) = s.Equals(value, opt)
        let replace (oldValue: string) (newValue: string) (s: string) = s.Replace(oldValue, newValue)
        let fromIndex (index: int) (s: string) = s.Substring(index)
        let upToIndex (index: int) (s: string) = s.Substring(0, index)

    module Seq =
        let rec cycle source =
            seq {
                yield! source
                yield! cycle source
            }

    let
#if !DEBUG
        inline
#endif
        alwaysTrue
            _
            =
        true

    let (|Ascending|) (x, y) =
        if x < y then struct (x, y) else struct (y, x)

    let (|Negative|_|) (value: int) = value < 0

    [<return: Struct>]
    let (|Natural|_|) (value: int) =
        match value with
        | x when x > 0 -> ValueSome x
        | _ -> ValueNone

    // NOTE: Follow the naming style of built-in types.
    // fsharplint:disable-next-line
    type pseq<'T> = ParallelQuery<'T>

    module PSeq =
        let ofSeq (source: 'T seq) : 'T pseq = source.AsParallel().AsOrdered()

        let filter predicate (source: 'T pseq) : 'T pseq =
            ParallelEnumerable.Where(source, Func<_, _>(predicate))

        let length (source: 'T pseq) : int = ParallelEnumerable.Count(source)

        let empty<'T> : 'T pseq = ParallelEnumerable.Empty<'T>()

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
                    | p ->
                        try
                            p.Value
                        with _ ->
                            // TODO: error occurs on Linux when accessing Linux-dependent properties. currently, suppress the error.
                            // Exception getting "Size": "There is no Runspace available to run scripts in this thread. You can provide one in the DefaultRunspace property of the System.Management.Automation.Runspaces.Runspace type. The script block you attempted to invoke was: $this.UnixStat.Size"
                            null

    let
#if !DEBUG
        inline
#endif
        (||*>)
            struct (a, b)
            f
            =
        f a b

    let
#if !DEBUG
        inline
#endif
        fst'
            struct (a, _)
            =
        a

    let
#if !DEBUG
        inline
#endif
        snd'
            struct (_, b)
            =
        b

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
                    | Obj o -> o[prop]
                    | Dict d -> d[prop]

    let unwrap (entries: Entry seq) : obj seq =
        entries
        |> Seq.map (function
            | Entry.Dict dct -> dct
            | Entry.Obj o -> o)

    let generateDictOfEnum<'ENUM when 'ENUM :> Enum> () =
        let dict = Generic.Dictionary<string, 'ENUM>(StringComparer.OrdinalIgnoreCase)

        Enum.GetValues typeof<'ENUM> :?> 'ENUM array
        |> Array.fold
            (fun (acc: Generic.Dictionary<string, 'ENUM>) k ->
                acc.Add(k.ToString(), k)
                acc)
            dict

    let generateArrayOfDu aType excludes =
        FSharpType.GetUnionCases aType
        |> fun arr ->
            match Set.isEmpty excludes with
            | true -> arr
            | _ -> arr |> Array.filter (fun u -> Set.contains u.Name excludes |> not)

    let makeUnion<'DU> (u: UnionCaseInfo) = FSharpValue.MakeUnion(u, [||]) :?> 'DU

    let generateDictOfDu<'DU> (excludes: string Set) =
        let dict = Generic.Dictionary<string, 'DU> StringComparer.InvariantCultureIgnoreCase

        generateArrayOfDu typeof<'DU> <| excludes
        |> Array.fold
            (fun (acc: Generic.Dictionary<string, 'DU>) u ->
                acc.Add(u.Name, makeUnion<'DU> u)
                acc)
            dict

    let tryFromStringExcludes (dict: Generic.Dictionary<string, 'DU>) s =
        let aType = typeof<'DU>

        match dict.TryGetValue s with
        | true, du -> Ok du
        | _ -> Error <| $"Unknown %s{aType.Name} '%s{s}'."

    let private fromString<'DU> (dict: Generic.Dictionary<string, 'DU>) s =
        tryFromStringExcludes dict s
        |> function
            | Ok x -> x
            | Error e -> failwith e

    let private toString (x: 'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    [<return: Struct>]
    let (|Prefix|_|) (p: string) (s: string) =
        match String.startsWith p s with
        | true -> s |> String.fromIndex (String.length p) |> ValueSome
        | _ -> ValueNone

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
        let fromString =
            generateDictOfDu<Action> <| set [ nameof Action.AddQuery ]
            |> tryFromStringExcludes

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<Struct>]
    type Matcher =
        | Eq
        | Like
        | Match

        override __.ToString() = toString __ |> _.ToLower()

    [<RequireQualifiedAccess>]
    module Matcher =
        let fromString = generateDictOfDu<Matcher> <| set [] |> fromString<Matcher>

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<Struct>]
    type Operator =
        | And
        | Or

        override __.ToString() = toString __ |> _.ToLower()

    [<RequireQualifiedAccess>]
    module Operator =
        let fromString = generateDictOfDu<Operator> <| set [] |> fromString<Operator>

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<Struct>]
    type Layout =
        | TopDown
        | TopDownHalf
        | BottomUp
        | BottomUpHalf

    [<RequireQualifiedAccess>]
    module Layout =
        let fromString = generateDictOfDu<Layout> <| set [] |> fromString<Layout>

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<Struct>]
    type PropertySearch =
        | NoSearch
        | Search of keyword: string
        | Rotate of keyword: string * candidates: string seq

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<Struct>]
    type Refresh =
        | Required
        | NotRequired

    // NOTE: Comparison and Equality are required.
    [<Struct>]
    type KeyPattern = { Modifier: int; Key: ConsoleKey }

    [<NoComparison>]
    type InternalConfig =
        { Layout: Layout
          Keymaps: Map<KeyPattern, Action>
          NotInteractive: bool
          WordDelimiters: string
          Prompt: string
          PromptLength: int
          Properties: Generic.IReadOnlyCollection<string>
          PropertiesMap: Generic.IReadOnlyDictionary<string, string> }

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<Struct>]
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
    [<Struct>]
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

        [<return: Struct>]
        let (|OverQuery|_|) (query: string) (cursor: int) =
            let ql = String.length query

            match cursor with
            | x when x > ql -> ValueSome ql
            | _ -> ValueNone

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
            | x, y when x + y < 0 || x + y > String.length state.Query -> state.InputMode
            | _ ->
                match state.InputMode with
                | InputMode.Input -> 0
                | InputMode.Select s -> s
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
                let struct (si, c) =
                    match state.Cursor, state.Cursor - c with
                    | Ascending x -> x

                { state with
                    Query = state.Query.Remove(si, c - si)
                    Cursor = si
                    InputMode = InputMode.Input }

        let getCurrentProperty (state: QueryState) =
            let s =
                state.Query
                |> String.upToIndex state.Cursor
                |> fun x -> String.fromIndex <| x.LastIndexOf " " + 1 <| x

#if DEBUG
            Logger.LogFile [ $"Query '{state.Query}' Cursor '{state.Cursor}' string '{s}'" ]
#endif

            match s with
            | Prefix ":" p -> PropertySearch.Search p
            | _ -> PropertySearch.NoSearch

    [<NoComparison>]
    [<Struct>]
    type QueryCondition =
        { Matcher: Matcher
          Operator: Operator
          CaseSensitive: bool
          Invert: bool }

    module QueryCondition =
        let toString (condition: QueryCondition) =
            [ match condition.CaseSensitive with
              | true -> "c"
              | _ -> ""
              // NOTE: use ToString to avoid extra branches when calculating coverages.
              match condition.Matcher, condition.Invert with
              | Matcher.Eq, true -> "ne"
              | m, true -> "not" + m.ToString()
              | m, _ -> m.ToString()
              " "
              condition.Operator.ToString() ]
            |> String.concat ""

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
          SuppressProperties: bool
          Refresh: Refresh }

    module InternalState =
        let queryInfo (state: InternalState) (count: int) =
            $" %s{state.QueryCondition |> QueryCondition.toString} [%d{count}]"

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
            (prompt: int)
            (consoleWidth: int)
            =

            { QueryState = queryState
              QueryCondition = queryCondition
              PropertySearch = QueryState.getCurrentProperty queryState
              SuppressProperties = suppressProperties
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

    let initConfig (p: IncomingParameters) : InternalConfig * InternalState =
        let prompt = p.Prompt + anchor
        let promptLength = prompt |> String.length

        { Layout = Layout.fromString p.Layout
          Keymaps = p.Keymaps
          NotInteractive = p.NotInteractive
          WordDelimiters = p.WordDelimiters
          Prompt = prompt
          PromptLength = promptLength
          Properties = p.Properties
          PropertiesMap = p.PropertiesMap },
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
            promptLength
            p.ConsoleWidth
