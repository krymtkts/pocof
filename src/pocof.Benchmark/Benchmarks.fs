﻿module pocof.Benchmark

open BenchmarkDotNet.Attributes
open System.Management.Automation

open Pocof
open Pocof.Operator

[<MemoryDiagnoser>]
type Benchmarks() =
    let psObjects = seq { 1..1000000 } |> Seq.map (string >> PSObject.AsPSObject)

    let hashtables =
        seq { 1..1000000 }
        |> Seq.map (fun i ->
            let h = new OrderedHashtable()
            h.Add("a", i)
            h)

    let hashtablePsObjects = hashtables |> Seq.map PSObject.AsPSObject

    [<Benchmark>]
    member __.buildProperties_PSObject() =
        let properties: Pocof.PropertyStore = Pocof.PropertyStore()

        psObjects
        |> Seq.iter (Pocof.buildProperties properties.ContainsKey properties.Add)

    [<Benchmark>]
    member __.buildProperties_Hashtable() =
        let properties: Pocof.PropertyStore = Pocof.PropertyStore()

        hashtablePsObjects
        |> Seq.iter (Pocof.buildProperties properties.ContainsKey properties.Add)

    [<Benchmark>]
    member __.dynamicLookup_PSObject() =
        psObjects |> Seq.iter (fun o -> o ?-> "Length" |> ignore)

    [<Benchmark>]
    member __.dynamicLookup_Hashtable() =
        hashtables |> Seq.iter (fun o -> o ?=> "Key" |> ignore)
