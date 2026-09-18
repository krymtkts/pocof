module Inspector

open ObjectLayoutInspector

open Pocof

let private printLayout t =
    typeof<TypeLayout>.GetMethod("PrintLayout", [| typeof<bool> |])
    |> function
        | null -> ()
        | m -> m.MakeGenericMethod([| t |]).Invoke(null, [| true |]) |> ignore

let private typesInData =
    [
        typeof<Data.Entry>
        typeof<Data.Action>
        typeof<Data.Matcher>
        typeof<Data.Operator>
        typeof<Data.Layout>
        typeof<Data.PropertySearch>
        typeof<Data.Refresh>
        typeof<Data.KeyPattern>
        typeof<Data.InternalConfig>
        typeof<Data.InputMode>
        typeof<Data.QueryState>
        typeof<Data.QueryCondition>
        typeof<Data.InternalState>
        typeof<Data.IncomingParameters>
    ]

let private typesInKeys =
#if DEBUG
    [ typeof<Keys.KeyInfo> ]
#else
    []
#endif

let private typesInQuery = [ typeof<Data.QueryPart>; typeof<Data.QueryContext> ]

let private typesInPocof =
    [
        typeof<Pocof.LoopFixedArguments>
        typeof<Pocof.RenderEvent>
        typeof<Pocof.RenderMessage>
        typeof<Pocof.RenderProcess>
    ]

let printMemoryLayout (group: string array) =
    match group with
    | [||] ->
        stdout.WriteLine "Memory layout of Pocof.Data types:=============================="
        typesInData |> List.iter printLayout
        stdout.WriteLine "Memory layout of Pocof.Keys types:=============================="
        typesInKeys |> List.iter printLayout
        stdout.WriteLine "Memory layout of Pocof.Query types:============================="
        typesInQuery |> List.iter printLayout
        stdout.WriteLine "Memory layout of Pocof.Pocof types:============================="
        typesInPocof |> List.iter printLayout
    | group ->
        group
        |> Array.iter (fun g ->
            match g.ToLower() with
            | "data" ->
                stdout.WriteLine "Memory layout of Pocof.Data types:=============================="
                typesInData |> List.iter printLayout

            | "keys" ->
                stdout.WriteLine "Memory layout of Pocof.Keys types:=============================="
                typesInKeys |> List.iter printLayout

            | "query" ->
                stdout.WriteLine "Memory layout of Pocof.Query types:============================="
                typesInQuery |> List.iter printLayout

            | "pocof" ->
                stdout.WriteLine "Memory layout of Pocof.Pocof types:============================="
                typesInPocof |> List.iter printLayout

            | _ -> printfn "Unknown group: %s\n" g)
