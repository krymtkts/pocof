namespace pocof

open System
open System.Collections

module PocofAction =

    type private Modifires =
        | NoModifier
        | Modifier of ConsoleModifiers

    let private kp (x: Modifires) k : PocofData.KeyPattern =
        let m =
            match x with
            | NoModifier -> 0
            | Modifier m -> m.GetHashCode()

        { Modifier = m; Key = k }

    // Shorthands for defining the default keymap.
    let private nom = kp NoModifier
    let private alt = kp <| Modifier(ConsoleModifiers.Alt)
    let private ctrl = kp <| Modifier(ConsoleModifiers.Control)

    let private defaultKeymap =
        Map [ (nom ConsoleKey.Escape, PocofData.Cancel)
              (ctrl ConsoleKey.C, PocofData.Cancel)
              (nom ConsoleKey.Enter, PocofData.Finish)

              (nom ConsoleKey.LeftArrow, PocofData.BackwardChar)
              (nom ConsoleKey.RightArrow, PocofData.ForwardChar)
              (nom ConsoleKey.Home, PocofData.BeginningOfLine)
              (nom ConsoleKey.End, PocofData.EndOfLine)

              (nom ConsoleKey.Backspace, PocofData.DeleteBackwardChar)
              (nom ConsoleKey.Delete, PocofData.DeleteForwardChar)
              (alt ConsoleKey.U, PocofData.KillBeginningOfLine)
              (alt ConsoleKey.K, PocofData.KillEndOfLine)

              (alt ConsoleKey.R, PocofData.RotateMatcher)
              (alt ConsoleKey.L, PocofData.RotateOperator)
              (alt ConsoleKey.C, PocofData.ToggleCaseSensitive)
              (alt ConsoleKey.I, PocofData.ToggleInvertFilter)

              (ctrl ConsoleKey.Spacebar, PocofData.ToggleSuppressProperties)
              (nom ConsoleKey.UpArrow, PocofData.SelectUp)
              (nom ConsoleKey.DownArrow, PocofData.SelectDown)
              (nom ConsoleKey.PageUp, PocofData.ScrollPageUp)
              (nom ConsoleKey.PageDown, PocofData.ScrollPageDown)

              (nom ConsoleKey.Tab, PocofData.TabExpansion) ]

    let inline toEnum<'a when 'a :> Enum and 'a: struct and 'a: (new: unit -> 'a)> (k: string) =
        let ok, e = Enum.TryParse<'a>(k, true)
        if ok then Some e else None

    let toKeyPattern (s: string) =
        match s.Split('+') |> List.ofSeq |> List.rev with
        | [] -> failwith "Unreachable pass."
        | [ k ] ->
            match toEnum<ConsoleKey> k with
            | Some e -> Ok <| nom e
            | None -> Error <| sprintf "Unsupported key '%s'." k
        | k :: ms ->
            let ke = toEnum<ConsoleKey> k

            let me =
                ms
                |> List.map toEnum<ConsoleModifiers>
                |> List.fold
                    (fun acc e ->
                        match (acc, e) with
                        | (Some a, Some x) -> a ||| x.GetHashCode() |> Some
                        | _ -> None)
                    (Some 0)

            match (ke, me) with
            | (Some k, Some m) -> Ok { Modifier = m; Key = k }
            | _ -> Error <| sprintf "Unsupported combination '%s'." s

    let convertKeymaps (h: Hashtable) =
        match h with
        | null -> defaultKeymap
        | x ->
            x
            |> Seq.cast<DictionaryEntry>
            |> Seq.map (fun e ->
                let k = e.Key.ToString() |> toKeyPattern
                let v = e.Value.ToString() |> PocofData.Action.ofString

                match (k, v) with
                | (Ok kv, v) -> (kv, v)
                | (Error e, _) -> failwith e) // TODO: enhance error handling.
            |> Map

    type private KeyInfo =
        { Pattern: PocofData.KeyPattern
          KeyChar: char }

    type private Key =
        | Char of char
        | Control of ConsoleKey
        | Modifier of PocofData.KeyPattern
        | Shortcut of PocofData.Action

    let private key (getKey: unit -> ConsoleKeyInfo) =
        let k = getKey ()
        let m = k.Modifiers.GetHashCode()

        { KeyChar = k.KeyChar
          Pattern = { Modifier = m; Key = k.Key } }

    let private keyToAction (keymap: Map<PocofData.KeyPattern, PocofData.Action>) (key: KeyInfo) =
        if Map.containsKey key.Pattern defaultKeymap then
            Shortcut defaultKeymap.[key.Pattern]
        elif Map.containsKey key.Pattern keymap then // TODO; currently cannot overrides default keymap...
            Shortcut keymap.[key.Pattern]
        elif key.Pattern.Modifier > 0 then
            Modifier key.Pattern
        elif Char.IsControl(key.KeyChar) then
            Control key.Pattern.Key
        else
            Char key.KeyChar

    let get (keymap: Map<PocofData.KeyPattern, PocofData.Action>) (getKey: unit -> ConsoleKeyInfo) =
        match key getKey |> keyToAction keymap with
        | Char c -> PocofData.AddChar c
        | Control _
        | Modifier _ -> PocofData.None
        | Shortcut a -> a
