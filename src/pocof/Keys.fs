namespace Pocof

open System
open System.Collections

module Keys =
    [<RequireQualifiedAccess>]
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

    let private ctrl =
        modify
        <| Modifiers.Modifier ConsoleModifiers.Control

    let private shift =
        modify
        <| Modifiers.Modifier ConsoleModifiers.Shift

    let defaultKeymap =
        Map [ (plain ConsoleKey.Escape, Data.Action.Cancel)
              (ctrl ConsoleKey.C, Data.Action.Cancel)
              (plain ConsoleKey.Enter, Data.Action.Finish)

              (plain ConsoleKey.LeftArrow, Data.Action.BackwardChar)
              (plain ConsoleKey.RightArrow, Data.Action.ForwardChar)
              (plain ConsoleKey.Home, Data.Action.BeginningOfLine)
              (plain ConsoleKey.End, Data.Action.EndOfLine)

              (plain ConsoleKey.Backspace, Data.Action.DeleteBackwardChar)
              (plain ConsoleKey.Delete, Data.Action.DeleteForwardChar)
              (alt ConsoleKey.U, Data.Action.KillBeginningOfLine)
              (alt ConsoleKey.K, Data.Action.KillEndOfLine)

              (shift ConsoleKey.LeftArrow, Data.Action.SelectBackwardChar)
              (shift ConsoleKey.RightArrow, Data.Action.SelectForwardChar)
              (shift ConsoleKey.Home, Data.Action.SelectToBeginningOfLine)
              (shift ConsoleKey.End, Data.Action.SelectToEndOfLine)

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

    let toEnum<'a when 'a :> Enum and 'a: struct and 'a: (new: unit -> 'a)> (k: string) =
        match Enum.TryParse<'a>(k, true) with
        | (true, e) -> Some e
        | _ -> None

    let toKeyPattern (s: string) =
        match s |> String.split "+" |> List.ofSeq |> List.rev with
        | [] -> failwith "Unreachable pass."
        | [ k ] ->
            match toEnum<ConsoleKey> k with
            | Some e -> Ok <| plain e
            | None -> Error $"Unsupported key '%s{k}'."
        | k :: ms ->
            let k = toEnum<ConsoleKey> k

            let m =
                ms
                |> List.map toEnum<ConsoleModifiers>
                |> List.fold
                    (fun acc e ->
                        match (acc, e) with
                        | (Some a, Some x) -> a ||| x.GetHashCode() |> Some
                        | _ -> None)
                    (Some 0)

            match (k, m) with
            | (Some k, Some m) -> Ok { Modifier = m; Key = k }
            | _ -> Error $"Unsupported combination '%s{s}'."

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
                        | Ok (o) -> (o :: fst, snd)
                        | Error e -> (fst, e :: snd))
                    ([], [])

            match ok, ng with
            | c, [] ->
                let source = defaultKeymap |> Map.toList
                List.append source c |> Map.ofSeq |> Ok
            | _, e -> e |> List.rev |> String.concat "\n" |> Error


    type private KeyInfo =
        { Pattern: Data.KeyPattern
          KeyChar: char }

    type private Key =
        | Char of char
        | Control of ConsoleKey
        | Shortcut of Data.Action

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
        | ShortcutKey keymap k -> Shortcut k
        | ControlKey c -> Control c
        | _ -> Char key.KeyChar

    let get (keymap: Map<Data.KeyPattern, Data.Action>) (keyInfo: ConsoleKeyInfo list) =
        keyInfo
        |> List.map (key >> keyToAction keymap)
        |> List.fold
            (fun acc x ->
                (acc, x)
                |> function
                    | Data.Action.AddQuery s, Char c -> string c |> (+) s |> Data.Action.AddQuery
                    | _, Char c -> Data.Action.AddQuery <| string c
                    | _, Shortcut a -> a
                    | _, Control _ -> Data.Action.Noop)
            Data.Action.Noop
