open BenchmarkDotNet.Running
open pocof.Benchmark

[<EntryPoint>]
let main argv =
    BenchmarkSwitcher.FromTypes([| typeof<Benchmarks>; typeof<KeysBenchmarks> |]).Run(argv)
    |> ignore

    0 // return an integer exit code
