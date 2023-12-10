namespace pocof

open System
open System.Collections

module PocofAction =

    type private Modifiers =
        | Plain
        | Modifier of ConsoleModifiers

    let private modify (x: Modifiers) k : PocofData.KeyPattern =
        let m =
            match x with
            | Plain -> 0
            | Modifier m -> m.GetHashCode()

        { Modifier = m; Key = k }

    // Shorthands for defining the default key-map.
    let private plain = modify Plain
    let private alt = modify <| Modifier ConsoleModifiers.Alt
    let private ctrl = modify <| Modifier ConsoleModifiers.Control

    let defaultKeymap =
        Map [ (plain ConsoleKey.Escape, PocofData.Cancel)
              (ctrl ConsoleKey.C, PocofData.Cancel)
              (plain ConsoleKey.Enter, PocofData.Finish)

              (plain ConsoleKey.LeftArrow, PocofData.BackwardChar)
              (plain ConsoleKey.RightArrow, PocofData.ForwardChar)
              (plain ConsoleKey.Home, PocofData.BeginningOfLine)
              (plain ConsoleKey.End, PocofData.EndOfLine)

              (plain ConsoleKey.Backspace, PocofData.DeleteBackwardChar)
              (plain ConsoleKey.Delete, PocofData.DeleteForwardChar)
              (alt ConsoleKey.U, PocofData.KillBeginningOfLine)
              (alt ConsoleKey.K, PocofData.KillEndOfLine)

              (alt ConsoleKey.R, PocofData.RotateMatcher)
              (alt ConsoleKey.L, PocofData.RotateOperator)
              (alt ConsoleKey.C, PocofData.ToggleCaseSensitive)
              (alt ConsoleKey.I, PocofData.ToggleInvertFilter)

              (ctrl ConsoleKey.Spacebar, PocofData.ToggleSuppressProperties)
              (plain ConsoleKey.UpArrow, PocofData.SelectUp)
              (plain ConsoleKey.DownArrow, PocofData.SelectDown)
              (plain ConsoleKey.PageUp, PocofData.ScrollPageUp)
              (plain ConsoleKey.PageDown, PocofData.ScrollPageDown)

              (plain ConsoleKey.Tab, PocofData.CompleteProperty) ]

    let inline toEnum<'a when 'a :> Enum and 'a: struct and 'a: (new: unit -> 'a)> (k: string) =
        match Enum.TryParse<'a>(k, true) with
        | (true, e) -> Some e
        | _ -> None

    let toKeyPattern (s: string) =
        match s.Split '+' |> List.ofSeq |> List.rev with
        | [] -> failwith "Unreachable pass."
        | [ k ] ->
            match toEnum<ConsoleKey> k with
            | Some e -> Ok <| plain e
            | None -> Error <| sprintf "Unsupported key '%s'." k
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
            | _ -> Error <| sprintf "Unsupported combination '%s'." s

    let convertKeymaps (h: Hashtable) =
        match h with
        | null -> defaultKeymap
        | x ->
            let source = defaultKeymap |> Map.toSeq

            let custom =
                x
                |> Seq.cast<DictionaryEntry>
                |> Seq.map (fun e ->
                    let k = string e.Key |> toKeyPattern
                    let v = string e.Value |> PocofData.Action.fromString

                    match (k, v) with
                    | (Ok kv, Ok av) -> (kv, av)
                    // TODO: enhance error handling.
                    | (Error e1, Error e2) -> failwith <| e1 + e2
                    | (Error e, _)
                    | (_, Error e) -> failwith e)

            Seq.append source custom |> Map.ofSeq

    type private KeyInfo =
        { Pattern: PocofData.KeyPattern
          KeyChar: char }

    type private Key =
        | Char of char
        | Control of ConsoleKey
        | Shortcut of PocofData.Action

    let private key (k: ConsoleKeyInfo) =
        let m = k.Modifiers.GetHashCode()

        { KeyChar = k.KeyChar
          Pattern = { Modifier = m; Key = k.Key } }

    let private (|ShortcutKey|_|) (m: Map<PocofData.KeyPattern, PocofData.Action>) (k: KeyInfo) =
        match Map.tryFind k.Pattern m with
        | Some v -> Some v
        | _ -> None

    let private (|ControlKey|_|) (k: KeyInfo) =
        match Char.IsControl k.KeyChar with
        | true -> Some k.Pattern.Key
        | _ -> None

    let private keyToAction (keymap: Map<PocofData.KeyPattern, PocofData.Action>) (key: KeyInfo) =
        match key with
        | ShortcutKey keymap k -> Shortcut k
        | ControlKey c -> Control c
        | _ -> Char key.KeyChar

    let get (keymap: Map<PocofData.KeyPattern, PocofData.Action>) (keyInfo: ConsoleKeyInfo list) =
        keyInfo
        |> List.map (key >> keyToAction keymap)
        |> List.fold
            (fun acc x ->
                (acc, x)
                |> function
                    | PocofData.AddChar s, Char c -> string c |> (+) s |> PocofData.AddChar
                    | _, Char c -> PocofData.AddChar <| string c
                    | _, Shortcut a -> a
                    | _, Control _ -> PocofData.Noop)
            PocofData.Noop
