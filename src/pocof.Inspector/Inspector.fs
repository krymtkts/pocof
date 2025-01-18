module Inspector

open ObjectLayoutInspector

open Pocof

let private printLayout t =
    typeof<TypeLayout>.GetMethod("PrintLayout", [| typeof<bool> |]).MakeGenericMethod([| t |]).Invoke(null, [| true |])
    |> ignore

let private typesInData =
    [ typeof<Data.Entry>
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
      typeof<Data.IncomingParameters> ]

let private typesInKeys =
#if DEBUG
    [ typeof<Keys.KeyInfo> ]
#else
    []
#endif

let private typesInQuery = [ typeof<Query.QueryPart>; typeof<Query.QueryContext> ]

let private typesInPocof =
    [ typeof<Pocof.LoopFixedArguments>
      typeof<Pocof.RenderEvent>
      typeof<Pocof.RenderMessage>
      typeof<Pocof.RenderProcess> ]

let printMemoryLayout (group: string array) =
    match group with
    | [||] ->
        printfn "Memory layout of Pocof.Data types:==============================\n"
        typesInData |> List.iter printLayout
        printfn "Memory layout of Pocof.Keys types:==============================\n"
        typesInKeys |> List.iter printLayout
        printfn "Memory layout of Pocof.Query types:=============================\n"
        typesInQuery |> List.iter printLayout
        printfn "Memory layout of Pocof.Pocof types:=============================\n"
        typesInPocof |> List.iter printLayout
    | group ->
        group
        |> Array.iter (fun g ->
            match g.ToLower() with
            | "data" ->
                printfn "Memory layout of Pocof.Data types:==============================\n"
                typesInData |> List.iter printLayout

            | "keys" ->
                printfn "Memory layout of Pocof.Keys types:==============================\n"
                typesInKeys |> List.iter printLayout

            | "query" ->
                printfn "Memory layout of Pocof.Query types:=============================\n"
                typesInQuery |> List.iter printLayout

            | "pocof" ->
                printfn "Memory layout of Pocof.Pocof types:=============================\n"
                typesInPocof |> List.iter printLayout

            | _ -> printfn "Unknown group: %s\n" g)
