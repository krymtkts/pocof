module Inspector

open ObjectLayoutInspector

open Pocof

let private printLayout t =
    typeof<TypeLayout>.GetMethod("PrintLayout", [| typeof<bool> |]).MakeGenericMethod([| t |]).Invoke(null, [| true |])
    |> ignore

let printMemoryLayout () =
    printfn "Memory layout of Pocof.Data types:=============================="

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
    |> List.iter printLayout
