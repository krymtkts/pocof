namespace pocof

open System

module PocofConsole =
    type KeyState =
        val caAsInput: bool
        new(s) = { caAsInput = s }

        interface IDisposable with
            member __.Dispose() =
                Console.TreatControlCAsInput <- __.caAsInput

    let init =
        let state = new KeyState(Console.TreatControlCAsInput)
        Console.TreatControlCAsInput <- true
        state

module PocofAction =
    type code =
        | None
        | Cancel
        | Finish

        | BackwardChar
        | ForwardChar

        | BeginningOfLine
        | EndOfLine

        | AddChar
        | DeleteBackwardChar
        | DeleteForwardChar
        | DeleteBackwardWord

        | KillBeginningOfLine
        | KillEndOfLine
        // toggle options.
        | RotateMatcher
        | ToggleCaseSensitive
        | ToggleInvertFilter
        | ToggleSelectionAndSelectNext
        // move selection.
        | SelectUp
        | SelectDown
        | ScrollPageUp
        | ScrollPageDown
        // autocomplete?
        | TabExpansion

    let keyMap =
        // TODO: cannot use map literal with Ionide.
        // TODO: change more better type structure.
        [ ("Escape", Cancel)
          ("Control+C", Cancel)
          ("Enter", Finish)
          ("Alt+B", BackwardChar)
          ("Alt+F", ForwardChar)
          ("Alt+A", BeginningOfLine)
          ("Alt+E", EndOfLine)
          ("Backspace", DeleteBackwardChar)
          ("Delete", DeleteForwardChar)
          ("Alt+U", KillBeginningOfLine)
          ("Alt+K", KillEndOfLine)
          ("Alt+R", RotateMatcher)
          ("Alt+C", ToggleCaseSensitive)
          ("Alt+I", ToggleInvertFilter)
          ("Alt+W", DeleteBackwardWord) // ?
          ("Alt+N", SelectUp) // ?
          ("Alt+P", SelectDown) // ?
          ("Control+Spacebar", ToggleSelectionAndSelectNext) // ?
          ("UpArrow", SelectUp) // ?
          ("DownArrow", SelectDown) // ?
          ("RightArrow", ScrollPageUp) // ?
          ("LeftArrow", ScrollPageDown) // ?
          ("Tab", TabExpansion) ] // ?
        |> Map.ofSeq

    let get () =
        use ks = PocofConsole.init
        // TODO: implement get action from key input.
        let k = Console.ReadKey true

        let kstr =
            if ConsoleModifiers.IsDefined k.Modifiers then
                k.Modifiers.ToString().Replace(",", "+")
                + k.Key.ToString()
            else
                k.Key.ToString()

        if Map.containsKey kstr keyMap then
            keyMap.[kstr]
        else
            AddChar
