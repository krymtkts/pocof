namespace pocof

open System

module PocofAction =
    let internal keyMap =
        // TODO: cannot use map literal with Ionide.
        // TODO: change more better type structure.
        [ ("Escape", PocofData.Cancel)
          ("Control+C", PocofData.Cancel)
          ("Enter", PocofData.Finish)

          ("LeftArrow", PocofData.BackwardChar)
          ("RightArrow", PocofData.ForwardChar)
          ("Home", PocofData.BeginningOfLine)
          ("End", PocofData.EndOfLine)

          ("Backspace", PocofData.DeleteBackwardChar)
          ("Delete", PocofData.DeleteForwardChar)
          ("Alt+U", PocofData.KillBeginningOfLine)
          ("Alt+K", PocofData.KillEndOfLine)

          ("Alt+R", PocofData.RotateMatcher)
          ("Alt+L", PocofData.RotateOperator)
          ("Alt+C", PocofData.ToggleCaseSensitive)
          ("Alt+I", PocofData.ToggleInvertFilter)

          ("Control+Spacebar", PocofData.ToggleSuppressProperties)
          ("UpArrow", PocofData.SelectUp)
          ("DownArrow", PocofData.SelectDown)
          ("PageUp", PocofData.ScrollPageUp)
          ("PageDown", PocofData.ScrollPageDown)

          ("Tab", PocofData.TabExpansion) ]
        |> Map.ofSeq

    let get (userKeymap: Map<String, PocofData.Action>) (getKey: unit -> ConsoleKeyInfo) =
        let k = getKey ()

        let kstr =
            let m =
                [ ConsoleModifiers.Alt
                  ConsoleModifiers.Shift
                  ConsoleModifiers.Control ]
                |> List.exists k.Modifiers.HasFlag

            if m then
                k.Modifiers.ToString().Replace(", ", "+")
                + "+"
                + k.Key.ToString()
            else
                k.Key.ToString()

        if Map.containsKey kstr userKeymap then
            userKeymap.[kstr]
        elif Map.containsKey kstr keyMap then
            keyMap.[kstr]
        elif
            k.Modifiers.HasFlag ConsoleModifiers.Alt
            || k.Modifiers.HasFlag ConsoleModifiers.Control // NOTE: block non-shift modifiers.
            || Char.IsControl(k.KeyChar)
        then
            PocofData.None
        else
            PocofData.AddChar k.KeyChar
