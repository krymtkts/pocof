namespace Pocof

open System
open System.Collections

open Data

module Keys =
    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<NoEquality>]
    [<Struct>]
    type private Modifiers =
        | Plain
        | Modifier of ConsoleModifiers

    let private modify (x: Modifiers) k : KeyPattern =
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

    let private ctlSft =
        modify
        <| Modifiers.Modifier(ConsoleModifiers.Control ||| ConsoleModifiers.Shift)

    let defaultKeymap =
        Map
            [ (plain ConsoleKey.Escape, Action.Cancel)
              (ctrl ConsoleKey.C, Action.Cancel)
              (plain ConsoleKey.Enter, Action.Finish)

              (plain ConsoleKey.LeftArrow, Action.BackwardChar)
              (ctrl ConsoleKey.LeftArrow, Action.BackwardWord)
              (plain ConsoleKey.RightArrow, Action.ForwardChar)
              (ctrl ConsoleKey.RightArrow, Action.ForwardWord)
              (plain ConsoleKey.Home, Action.BeginningOfLine)
              (plain ConsoleKey.End, Action.EndOfLine)

              (plain ConsoleKey.Backspace, Action.DeleteBackwardChar)
              (plain ConsoleKey.Delete, Action.DeleteForwardChar)
              (ctrl ConsoleKey.Backspace, Action.DeleteBackwardWord)
              (ctrl ConsoleKey.Delete, Action.DeleteForwardWord)
              (ctrl ConsoleKey.Home, Action.DeleteBackwardInput)
              (ctrl ConsoleKey.End, Action.DeleteForwardInput)

              (shift ConsoleKey.LeftArrow, Action.SelectBackwardChar)
              (shift ConsoleKey.RightArrow, Action.SelectForwardChar)
              (ctlSft ConsoleKey.LeftArrow, Action.SelectBackwardWord)
              (ctlSft ConsoleKey.RightArrow, Action.SelectForwardWord)
              (shift ConsoleKey.Home, Action.SelectToBeginningOfLine)
              (shift ConsoleKey.End, Action.SelectToEndOfLine)
              (ctrl ConsoleKey.A, Action.SelectAll)

              (alt ConsoleKey.R, Action.RotateMatcher)
              (alt ConsoleKey.L, Action.RotateOperator)
              (alt ConsoleKey.C, Action.ToggleCaseSensitive)
              (alt ConsoleKey.I, Action.ToggleInvertFilter)

              (ctrl ConsoleKey.Spacebar, Action.ToggleSuppressProperties)
              (plain ConsoleKey.UpArrow, Action.SelectLineUp)
              (plain ConsoleKey.DownArrow, Action.SelectLineDown)
              (plain ConsoleKey.PageUp, Action.ScrollPageUp)
              (plain ConsoleKey.PageDown, Action.ScrollPageDown)

              (plain ConsoleKey.Tab, Action.CompleteProperty) ]

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

    let convertKeymaps (h: Hashtable | null) =
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
    type private KeyInfo = { Pattern: KeyPattern; KeyChar: char }

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    [<NoEquality>]
    [<Struct>]
    type private Key =
        | Char of c: char
        | Control of key: ConsoleKey
        | Shortcut of action: Action

    let private key (k: ConsoleKeyInfo) =
        let m = k.Modifiers.GetHashCode()

        { KeyChar = k.KeyChar
          Pattern = { Modifier = m; Key = k.Key } }

    let private (|ShortcutKey|_|) (m: Map<KeyPattern, Action>) (k: KeyInfo) =
        match Map.tryFind k.Pattern m with
        | Some v -> Some v
        | _ -> None

    let private (|ControlKey|_|) (k: KeyInfo) =
        match Char.IsControl k.KeyChar with
        | true -> Some k.Pattern.Key
        | _ -> None

    let private keyToAction (keymap: Map<KeyPattern, Action>) (key: KeyInfo) =
        match key with
        | ShortcutKey keymap k -> Key.Shortcut k
        | ControlKey c -> Key.Control c
        | _ -> Key.Char key.KeyChar

    let get (keymap: Map<KeyPattern, Action>) (keyInfo: ConsoleKeyInfo list) =
        keyInfo
        |> List.map (key >> keyToAction keymap)
        |> List.fold
            (fun acc x ->
                (acc, x)
                |> function
                    | Action.AddQuery s, Key.Char c -> string c |> (+) s |> Action.AddQuery
                    | _, Key.Char c -> Action.AddQuery <| string c
                    | _, Key.Shortcut a -> a
                    | _, Key.Control _ -> Action.Noop)
            Action.Noop
