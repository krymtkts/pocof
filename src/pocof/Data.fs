namespace pocof

open System
open System.Management.Automation
open System.Collections
open Microsoft.FSharp.Reflection

// for debugging.
module PocofDebug =
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
                |> List.iter (fun r ->
                    fprintfn
                        sw
                        "[%s] %s at %d %s <%A>"
                        (DateTimeOffset.Now.ToString("yyyy-MM-dd'T'HH:mm:ss.fffzzz"))
                        path
                        line
                        caller
                        r))

module PocofData =
    type Entry =
        | Obj of PSObject
        | Dict of DictionaryEntry

    let unwrap (entries: Entry list) =
        entries
        |> List.map (fun o ->
            match o with
            | Dict (dct) -> dct :> obj
            | Obj (o) -> o)

    type String with
        static member lower(s: string) = s.ToLower()
        static member upper(s: string) = s.ToUpper()

    let private tryFromStringExcludes<'a> (excludes: Set<string>) s =
        let name = String.lower s

        match FSharpType.GetUnionCases typeof<'a>
              |> Seq.filter (fun u -> Set.contains u.Name excludes |> not)
              |> Seq.tryFind (fun u -> u.Name |> String.lower = name)
            with
        | Some u -> Ok <| (FSharpValue.MakeUnion(u, [||]) :?> 'a)
        | _ -> Error <| sprintf "Unknown case '%s'." s

    let private fromString<'a> s =
        let name = String.lower s

        match FSharpType.GetUnionCases typeof<'a>
              |> Seq.tryFind (fun u -> u.Name |> String.lower = name)
            with
        | Some u -> FSharpValue.MakeUnion(u, [||]) :?> 'a
        | _ -> failwithf "Unknown case '%s'." s

    let private toString (x: 'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    type Action =
        | Noop
        | Cancel
        | Finish
        // move.
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
        // toggle options.
        | RotateMatcher
        | RotateOperator
        | ToggleCaseSensitive
        | ToggleInvertFilter
        | ToggleSuppressProperties
        // move selection.
        | SelectUp
        | SelectDown
        | ScrollPageUp
        | ScrollPageDown
        // autocomplete
        | CompleteProperty
        static member fromString =
            tryFromStringExcludes<Action>
            <| set [ "AddQuery" ]

    type Matcher =
        | EQ
        | LIKE
        | MATCH
        static member fromString = fromString<Matcher>
        override __.ToString() = toString __ |> String.lower

    type Operator =
        | AND
        | OR
        | NONE
        static member fromString = fromString<Operator>
        override __.ToString() = toString __ |> String.lower

    type Layout =
        | TopDown
        | BottomUp
        static member fromString = fromString<Layout>

    type PropertySearch =
        | NonSearch
        | Search of string
        | Rotate of string * int * string list

    type Refresh =
        | Required
        | NotRequired
        static member ofBool =
            function
            | true -> Required
            | _ -> NotRequired

    type KeyPattern = { Modifier: int; Key: ConsoleKey }

    type InternalConfig =
        { Prompt: string
          Layout: Layout
          Keymaps: Map<KeyPattern, Action>
          NotInteractive: bool }

    type QueryState =
        { Matcher: Matcher
          Operator: Operator
          CaseSensitive: bool
          Invert: bool }
        override __.ToString() =
            List.append
            <| match __.Matcher, __.CaseSensitive, __.Invert with
               | EQ, true, true -> [ "cne" ]
               | EQ, false, true -> [ "ne" ]
               | m, true, true -> [ "notc"; string m ]
               | m, true, false -> [ "c"; string m ]
               | m, false, true -> [ "not"; string m ]
               | m, false, false -> [ string m ]
            <| [ " "; string __.Operator ]
            |> String.concat ""

    type InternalState =
        { Query: string
          QueryState: QueryState
          PropertySearch: PropertySearch
          Notification: string
          SuppressProperties: bool }

    type Position = { X: int; Y: int }

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
          Keymaps: Map<KeyPattern, Action> }

    let (|Prefix|_|) (p: string) (s: string) =
        match s.StartsWith p with
        | true -> Some s.[1..]
        | _ -> None

    let private getCurrentProperty (query: string) (x: int) =
        let s = query.[..x].Split [| ' ' |] |> Seq.last

        match s with
        | Prefix ":" p -> Search <| p
        | _ -> NonSearch

    let initConfig (p: IncomingParameters) =
        // TODO: Eliminate the possibility of failure from here.
        { Prompt = p.Prompt
          Layout = Layout.fromString p.Layout
          Keymaps = p.Keymaps
          NotInteractive = p.NotInteractive },
        { Query = p.Query
          QueryState =
            { Matcher = Matcher.fromString p.Matcher
              Operator = Operator.fromString p.Operator
              CaseSensitive = p.CaseSensitive
              Invert = p.InvertQuery }
          PropertySearch = getCurrentProperty p.Query p.Query.Length
          Notification = ""
          SuppressProperties = p.SuppressProperties },
        { X = p.Query.Length; Y = 0 }

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
        { state with PropertySearch = NonSearch },
        { pos with X = 0 },
        (pos.X <> 0 && state.PropertySearch <> NonSearch)
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
        | NonSearch -> state, pos, NotRequired
        | Search keyword ->
            let candidate, candidates =
                props
                |> List.filter (fun p -> p.ToLower().StartsWith(keyword.ToLower()))
                |> function
                    | [] -> "", []
                    | xs -> List.head xs, xs

            match candidate with
            | "" -> state, pos, NotRequired
            | _ ->
                let basePosition, head, tail = splitQuery keyword
#if DEBUG
                PocofDebug.Logger.logFile [ $"Search keyword '{keyword}' head '{head}' candidate '{candidate}' tail '{tail}'" ]
#endif
                buildValues head candidate tail keyword 0 candidates basePosition
        | Rotate (keyword, i, candidates) ->
            let cur = candidates.[i]
            let i = (i + 1) % candidates.Length
            let next = candidates.[i]
            let basePosition, head, tail = splitQuery cur
#if DEBUG
            PocofDebug.Logger.logFile [ $"Rotate keyword '{keyword}' head '{head}' cur '{cur}' next '{next}' tail '{tail}'" ]
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
