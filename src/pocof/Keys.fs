namespace Pocof

open System
open System.Collections

module Keys =
    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<NoEquality>]
    type private Modifiers =
        | Plain
        | Modifier of ConsoleModifiers

    let private modify (x: Modifiers) k : Data.KeyPattern =
        let m =
            match x with
            | Modifiers.Plain -> 0
            | Modifiers.Modifier m -> m.GetHashCode()

        { Modifier = m; Key = k }

    // Shorthands for defining the default key-map.
    let private plain = modify Modifiers.Plain
    let private alt = modify <| Modifiers.Modifier ConsoleModifiers.Alt

    let private ctrl = modify <| Modifiers.Modifier ConsoleModifiers.Control

    let private shift = modify <| Modifiers.Modifier ConsoleModifiers.Shift

    let defaultKeymap =
        Map
            [ (plain ConsoleKey.Escape, Data.Action.Cancel)
              (ctrl ConsoleKey.C, Data.Action.Cancel)
              (plain ConsoleKey.Enter, Data.Action.Finish)

              (plain ConsoleKey.LeftArrow, Data.Action.BackwardChar)
              (plain ConsoleKey.RightArrow, Data.Action.ForwardChar)
              (plain ConsoleKey.Home, Data.Action.BeginningOfLine)
              (plain ConsoleKey.End, Data.Action.EndOfLine)

              (plain ConsoleKey.Backspace, Data.Action.DeleteBackwardChar)
              (plain ConsoleKey.Delete, Data.Action.DeleteForwardChar)
              (ctrl ConsoleKey.Home, Data.Action.DeleteBackwardInput)
              (ctrl ConsoleKey.End, Data.Action.DeleteForwardInput)

              (shift ConsoleKey.LeftArrow, Data.Action.SelectBackwardChar)
              (shift ConsoleKey.RightArrow, Data.Action.SelectForwardChar)
              (shift ConsoleKey.Home, Data.Action.SelectToBeginningOfLine)
              (shift ConsoleKey.End, Data.Action.SelectToEndOfLine)
              (ctrl ConsoleKey.A, Data.Action.SelectAll)

              (alt ConsoleKey.R, Data.Action.RotateMatcher)
              (alt ConsoleKey.L, Data.Action.RotateOperator)
              (alt ConsoleKey.C, Data.Action.ToggleCaseSensitive)
              (alt ConsoleKey.I, Data.Action.ToggleInvertFilter)

              (ctrl ConsoleKey.Spacebar, Data.Action.ToggleSuppressProperties)
              (plain ConsoleKey.UpArrow, Data.Action.SelectLineUp)
              (plain ConsoleKey.DownArrow, Data.Action.SelectLineDown)
              (plain ConsoleKey.PageUp, Data.Action.ScrollPageUp)
              (plain ConsoleKey.PageDown, Data.Action.ScrollPageDown)

              (plain ConsoleKey.Tab, Data.Action.CompleteProperty) ]

    let private toEnum<'a when 'a :> Enum and 'a: struct and 'a: (new: unit -> 'a)> (k: string) =
        match Enum.TryParse<'a>(k, true) with
        | (true, e) -> Some e
        | _ -> None

    [<TailCall>]
    let rec private processKeys (keys: string list) (result: Result<Data.KeyPattern, string>) =
        match keys with
        | [] -> result
        | [ k ] ->
            match result, toEnum<ConsoleKey> k with
            | Ok r, Some e -> { r with Key = e } |> Ok
            | Ok _, None -> Error $"Unsupported key '%s{k}'."
            | Error e, None -> Error $"%s{e} Unsupported key '%s{k}'."
            | Error _ as e, _ -> e
            |> processKeys []
        | m :: keys ->
            match result, toEnum<ConsoleModifiers> m with
            | Ok r, Some x ->
                { r with
                    Modifier = r.Modifier ||| x.GetHashCode() }
                |> Ok
            | Ok _, None -> Error $"Unsupported modifier '%s{m}'."
            | Error e, None -> Error $"%s{e} Unsupported modifier '%s{m}'."
            | Error _ as e, _ -> e
            |> processKeys keys

    let toKeyPattern (s: string) =
        s |> String.split "+" |> List.ofSeq |> processKeys
        <| Ok
            { Data.KeyPattern.Modifier = 0
              Data.KeyPattern.Key = ConsoleKey.NoName }

    let convertKeymaps (h: Hashtable) =
        match h with
        | null -> defaultKeymap |> Ok
        | x ->
            let ok, ng =
                x
                |> Seq.cast<DictionaryEntry>
                |> Seq.toList
                |> List.map (fun e ->
                    let k = string e.Key |> toKeyPattern
                    let v = string e.Value |> Data.Action.fromString

                    match (k, v) with
                    | (Ok kv, Ok av) -> Ok(kv, av)
                    | (Error e1, Error e2) -> e1 + e2 |> Error
                    | (Error e, _)
                    | (_, Error e) -> Error e)
                |> List.fold
                    (fun (fst, snd) o ->
                        match o with
                        | Ok(o) -> (o :: fst, snd)
                        | Error e -> (fst, e :: snd))
                    ([], [])

            match ok, ng with
            | c, [] ->
                let source = defaultKeymap |> Map.toList
                List.append source c |> Map.ofSeq |> Ok
            | _, e -> e |> List.rev |> String.concat "\n" |> Error


    [<NoComparison>]
    [<NoEquality>]
    [<Struct>]
    type private KeyInfo =
        { Pattern: Data.KeyPattern
          KeyChar: char }

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<NoEquality>]
    [<Struct>]
    type private Key =
        | Char of c: char
        | Control of key :ConsoleKey
        | Shortcut of action :Data.Action

    let private key (k: ConsoleKeyInfo) =
        let m = k.Modifiers.GetHashCode()

        { KeyChar = k.KeyChar
          Pattern = { Modifier = m; Key = k.Key } }

    let private (|ShortcutKey|_|) (m: Map<Data.KeyPattern, Data.Action>) (k: KeyInfo) =
        match Map.tryFind k.Pattern m with
        | Some v -> Some v
        | _ -> None

    let private (|ControlKey|_|) (k: KeyInfo) =
        match Char.IsControl k.KeyChar with
        | true -> Some k.Pattern.Key
        | _ -> None

    let private keyToAction (keymap: Map<Data.KeyPattern, Data.Action>) (key: KeyInfo) =
        match key with
        | ShortcutKey keymap k -> Key.Shortcut k
        | ControlKey c -> Key.Control c
        | _ -> Key.Char key.KeyChar

    let get (keymap: Map<Data.KeyPattern, Data.Action>) (keyInfo: ConsoleKeyInfo list) =
        keyInfo
        |> List.map (key >> keyToAction keymap)
        |> List.fold
            (fun acc x ->
                (acc, x)
                |> function
                    | Data.Action.AddQuery s, Key.Char c -> string c |> (+) s |> Data.Action.AddQuery
                    | _, Key.Char c -> Data.Action.AddQuery <| string c
                    | _, Key.Shortcut a -> a
                    | _, Key.Control _ -> Data.Action.Noop)
            Data.Action.Noop
