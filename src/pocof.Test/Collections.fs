module PocofTest.Collections

open Expecto
open Expecto.Flip

open Pocof
open System
open System.Collections
open System.Reflection

// Reflection helper to obtain internal segment capacity sequence
let private getSegmentCapacities (buf: SpscAppendOnlyBuffer<int>) =
    let t = buf.GetType()

    let headFieldRaw =
        t.GetField("head", BindingFlags.NonPublic ||| BindingFlags.Instance)

    let headField =
        match headFieldRaw with
        | null -> failwith "head field not found"
        | f -> f

    let headObjRaw = headField.GetValue(buf)

    let headSeg =
        match headObjRaw with
        | null -> failwith "head segment null"
        | o -> o :?> SpscSegment<int>

    let capacities = ResizeArray<int>()

    let rec loop (seg: SpscSegment<int>) =
        capacities.Add seg.Capacity

        match seg.Next with
        | null -> ()
        | next -> loop next

    loop headSeg
    capacities |> Seq.toList

[<Tests>]
let tests =
    testList
        "SpscAppendOnlyBuffer"
        [

          test "Empty buffer yields no enumeration and Count=0" {
              let buf = SpscAppendOnlyBuffer<int>()
              buf.Count |> Expect.equal "count is zero" 0
              let mutable e = buf.GetEnumerator()
              e.MoveNext() |> Expect.isFalse "MoveNext should be false on empty"
          }

          test "Add and enumerate within a single segment" {
              let buf = SpscAppendOnlyBuffer<int>()
              [ 0..9 ] |> List.iter buf.Add
              buf.Count |> Expect.equal "count matches" 10
              buf |> Expect.sequenceEqual "enumeration order" [ 0..9 ]
              getSegmentCapacities buf |> Expect.equal "only initial segment" [ 128 ]
          }

          test "Enumerate across segment boundary (128 + 1)" {
              let buf = SpscAppendOnlyBuffer<int>()

              // NOTE: total 129 items
              [ 0..128 ] |> List.iter buf.Add

              buf.Count |> Expect.equal "count matches" 129
              buf |> Seq.length |> Expect.equal "length" 129
              buf |> Seq.take 5 |> Expect.sequenceEqual "prefix" [ 0; 1; 2; 3; 4 ]

              buf
              |> Seq.skip 124
              |> Seq.take 5
              |> Expect.sequenceEqual "tail around boundary" [ 124; 125; 126; 127; 128 ]

              getSegmentCapacities buf
              |> Expect.equal "two segments (128 -> 256)" [ 128; 256 ]
          }

          test "Capacity growth and max cap (128,256,512,1024,1024)" {
              let buf = SpscAppendOnlyBuffer<int>()

              // NOTE: Add 2944 items to allocate 5 segments (128 + 256 + 512 + 1024 + 1024)
              [ 0..2943 ] |> List.iter buf.Add
              buf.Count |> Expect.equal "count matches" 2944
              let caps = getSegmentCapacities buf
              caps |> Expect.equal "capacity growth with cap" [ 128; 256; 512; 1024; 1024 ]

              // NOTE: Add some more items; last capacity should remain 1024
              [ 2944..3000 ] |> List.iter buf.Add
              let caps2 = getSegmentCapacities buf

              // NOTE: An extra 1024-capacity segment may appear; only assert cap does not exceed 1024
              caps2 |> List.last |> Expect.equal "last capacity stays 1024" 1024
          }

          test "Count increases monotonically" {
              let buf = SpscAppendOnlyBuffer<int>()

              for i in 1..50 do
                  buf.Add i
                  buf.Count |> Expect.equal "monotonic" i
          }

          test "Snapshot enumeration does not see later additions" {
              let buf = SpscAppendOnlyBuffer<int>()
              [ 0..9 ] |> List.iter buf.Add
              let mutable e = buf.GetEnumerator()
              let collected = System.Collections.Generic.List<int>()

              // NOTE: Consume 5 items
              [ 1..5 ]
              |> List.iter (fun _ ->
                  e.MoveNext() |> Expect.isTrue "should move"
                  collected.Add e.Current)

              // NOTE:Add 10 more items midway (10..19)
              [ 10..19 ] |> List.iter buf.Add

              while e.MoveNext() do
                  collected.Add e.Current

              collected |> Expect.sequenceEqual "snapshot only first 10" [ 0..9 ]
              buf.Count |> Expect.equal "buffer count advanced" 20
          }

          test "Integrity across multiple segment enumeration" {
              let buf = SpscAppendOnlyBuffer<int>()

              // NOTE: 128+256+512+604 (within 4th segment)
              let total = 1500
              [ 0..total ] |> List.iter buf.Add
              let xs = buf |> Seq.toArray
              xs |> Array.length |> Expect.equal "length" total

              // NOTE: Position checks (start / segment boundaries / end)
              xs.[0] |> Expect.equal "first" 0
              xs.[127] |> Expect.equal "end seg1" 127
              xs.[128] |> Expect.equal "start seg2" 128
              xs.[128 + 255] |> Expect.equal "end seg2" (128 + 255)
              xs.[128 + 256] |> Expect.equal "start seg3" (128 + 256)
              xs.[total - 1] |> Expect.equal "last" (total - 1)
          }

          test "IReadOnlyCollection Count matches after many adds" {
              let buf = SpscAppendOnlyBuffer<int>()
              // NOTE: spans into second segment
              [ 0..257 ] |> List.iter buf.Add
              let c1 = (buf :> Generic.IReadOnlyCollection<int>).Count
              c1 |> Expect.equal "count via interface" 258
              buf.Add 999
              let c2 = (buf :> Generic.IReadOnlyCollection<int>).Count
              c2 |> Expect.equal "count via interface after add" 259
          }

          test "Non-generic IEnumerable enumerates same snapshot" {
              let buf = SpscAppendOnlyBuffer<string>()
              [ 0..10 ] |> List.iter (fun i -> buf.Add(string i))
              let en = (buf :> IEnumerable).GetEnumerator()
              // NOTE: mutate after enumerator acquired
              [ 11..20 ] |> List.iter (fun i -> buf.Add(string i))
              let acc = ResizeArray<string>()

              while en.MoveNext() do
                  acc.Add(en.Current :?> string)

              acc
              |> Expect.sequenceEqual "only initial snapshot elements" ([ 0..10 ] |> List.map string)
          }

          test "Snapshot enumerator ignores later segment allocations" {
              let buf = SpscAppendOnlyBuffer<int>()
              // NOTE: fill first segment exactly
              [ 0..127 ] |> List.iter buf.Add
              // NOTE: acquire enumerator snapshot (count = 128)
              let mutable e = buf.GetEnumerator()
              // NOTE: now add enough to allocate two more segments (trigger growth 256 + 512)
              [ 128 .. 128 + 256 + 512 - 1 ] |> List.iter buf.Add

              // NOTE: consume enumerator; should see only first 128
              let seen = ResizeArray<int>()

              while e.MoveNext() do
                  seen.Add e.Current

              seen.Count |> Expect.equal "snapshot size" 128
              seen |> Seq.head |> Expect.equal "first" 0
              seen |> Seq.last |> Expect.equal "last of first segment" 127
              // NOTE: overall count advanced
              buf.Count |> Expect.equal "full count" (128 + 256 + 512)
          }

          test "Enumerator hits null-next branch (artificial total > capacity)" {
              // NOTE: Construct a single segment with capacity 4 but claim total=5 in enumerator
              let seg = SpscSegment<int>(4)
              let mutable e = new SpscSegmentEnumerator<int>(seg, seg.Capacity + 1)

              // NOTE: Move through existing capacity
              [ 1 .. seg.Capacity ]
              |> List.iter (fun _ -> e.MoveNext() |> Expect.isTrue "within capacity should be true")

              // NOTE: Next move triggers null-next branch (remaining>0, idx==cap, Next=null)
              e.MoveNext() |> Expect.isFalse "null-next branch returns false"
          }

          test "Enumerator Reset throws NotSupportedException" {
              let seg = SpscSegment<int>(1)
              let e = new SpscSegmentEnumerator<int>(seg, 0)

              (fun () -> (e :> IEnumerator).Reset())
              |> Expect.throwsT<NotSupportedException> "Reset must throw"
          } ]
