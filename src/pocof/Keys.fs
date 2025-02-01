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

              (plain ConsoleKey.Tab, Action.CompleteProperty) ]

    let private toEnum<'a when 'a :> Enum and 'a: struct and 'a: (new: unit -> 'a)> (k: string) =
        match Enum.TryParse<'a>(k, true) with
        | (true, e) -> Some e
        | _ -> None

    [<TailCall>]
    let rec private processKeys (keys: string array) l i (result: Result<Data.KeyPattern, string>) =
        if i = l then
            result
        else
            let k = keys[i]
            let i = i + 1

            if i = l then
                match result, toEnum<ConsoleKey> k with
                | Ok r, Some e -> { r with Key = e } |> Ok
                | Ok _, None -> Error $"Unsupported key '%s{k}'."
                | Error e, None -> Error $"%s{e} Unsupported key '%s{k}'."
                | Error _ as e, _ -> e
                |> processKeys keys l i
            else
                match result, toEnum<ConsoleModifiers> k with
                | Ok r, Some x ->
                    { r with
                        Modifier = r.Modifier ||| x.GetHashCode() }
                    |> Ok
                | Ok _, None -> Error $"Unsupported modifier '%s{k}'."
                | Error e, None -> Error $"%s{e} Unsupported modifier '%s{k}'."
                | Error _ as e, _ -> e
                |> processKeys keys l i

    let toKeyPattern (s: string) =
        let keys = s |> String.split "+"

        processKeys keys (Array.length keys) 0
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
                |> Seq.map (fun e ->
                    let k = string e.Key |> toKeyPattern
                    let v = string e.Value |> Data.Action.fromString

                    match (k, v) with
                    | (Ok kv, Ok av) -> Ok(kv, av)
                    | (Error e1, Error e2) -> e1 + e2 |> Error
                    | (Error e, _)
                    | (_, Error e) -> Error e)
                |> Seq.fold
                    (fun (fst, snd) ->
                        function
                        | Ok(o) -> (o :: fst, snd)
                        | Error e -> (fst, e :: snd))
                    ([], [])

            match ok, ng with
            | c, [] ->
                let source = defaultKeymap |> Map.toSeq
                Seq.append source c |> Map.ofSeq |> Ok
            | _, e -> e |> List.rev |> String.concat "\n" |> Error


    [<NoComparison>]
    [<NoEquality>]
    [<Struct>]
    type
#if !DEBUG
        private
#endif
        KeyInfo = { Pattern: KeyPattern; KeyChar: char }

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

    let get (keymap: Map<KeyPattern, Action>) (keyInfo: ConsoleKeyInfo list) =
        keyInfo
        |> List.map key
        |> List.fold
            (fun acc key ->
                match key with
                | ShortcutKey keymap a -> a
                | ControlKey _ -> Action.Noop
                | _ ->
                    match acc with
                    | Action.AddQuery s -> key.KeyChar |> string |> (+) s
                    | _ -> key.KeyChar |> string
                    |> Action.AddQuery)
            Action.Noop
