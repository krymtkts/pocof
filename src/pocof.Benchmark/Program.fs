open BenchmarkDotNet.Running
open pocof.Benchmark

[<EntryPoint>]
let main argv =
    BenchmarkSwitcher.FromAssembly(typeof<PocofBenchmarks>.Assembly).Run(argv)
    |> ignore

    0 // return an integer exit code
