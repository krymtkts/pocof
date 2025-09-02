namespace Pocof

open System
open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading

[<Sealed>]
type SpscSegment<'T>(capacity: int) =
    let items: 'T array = Array.zeroCreate capacity
    let mutable next: SpscSegment<'T> | null = null
    member __.Items = items
    member __.Capacity = items.Length
    // NOTE: Non-volatile reads/writes are safe once the reader has observed the published SpscAppendOnlyBuffer.count.
    member __.Next
        with get () = next
        and set v = next <- v

[<Struct>]
type SpscSegmentEnumerator<'T> =
    val mutable private remaining: int
    val mutable private seg: SpscSegment<'T>
    val mutable private items: 'T array
    val mutable private cap: int
    val mutable private idx: int
    val mutable private current: 'T

    new(head: SpscSegment<'T>, total: int) =
        let items = head.Items

        { remaining = total
          seg = head
          items = items
          cap = items.Length
          idx = 0
          current = Unchecked.defaultof<'T> }

    // NOTE: for F# pattern enumeration optimization (zero allocation via struct enumerator).
    member __.Current = __.current

    // NOTE: for F# pattern enumeration optimization (zero allocation via struct enumerator).
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member __.MoveNext() =
        if __.remaining <= 0 then
            false
        elif __.idx < __.cap then
            let i = __.idx
            __.current <- __.items[i]
            __.idx <- i + 1
            __.remaining <- __.remaining - 1
            true
        else
            match __.seg.Next with
            | null ->
                __.remaining <- 0
                false
            | next ->
                __.seg <- next
                let items = next.Items
                __.items <- items
                __.cap <- items.Length
                // NOTE: Always at least one item in the new segment (because remaining > 0).
                __.current <- __.items[0]
                __.idx <- 1
                __.remaining <- __.remaining - 1
                true

    // NOTE: No resources to release.
    member __.Dispose() = ()

    interface IEnumerator<'T> with
        member __.Current = __.current

    interface IEnumerator with
        member __.Current = box __.current
        member __.MoveNext() = __.MoveNext()
        member _.Reset() = raise (NotSupportedException())

    interface IDisposable with
        member __.Dispose() = __.Dispose()

// Single-producer/single-consumer append-only segmented buffer.
// - Writer: Enqueue only
// - Reader: Enumerate/Count only (snapshot iteration)
[<Sealed>]
type SpscAppendOnlyBuffer<'T>() =
    [<Literal>]
    let segSize = 128

    [<Literal>]
    let segSizeMax = 1024

    // NOTE: Head (first) and tail (current write) segments.
    let head = SpscSegment<'T>(segSize)
    let mutable tail = head
    let mutable tailIndex = 0 // next write index within tail

    // NOTE: Published element count (used for snapshot and IReadOnlyCollection.Count).
    let mutable count = 0

    // NOTE: Volatile helpers for count.
    let readCount () = Volatile.Read(&count)
    let writeCount (v: int) = Volatile.Write(&count, v)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member __.Add(item: 'T) : unit =
        // NOTE: Acquire local view of the current tail segment.
        let t = tail
        let idx = tailIndex
        let cap = t.Capacity

        if idx >= cap then
            // NOTE: Current segment is full: create a new one, link it, and advance tail.
            let newSeg = SpscSegment<'T>(min (cap <<< 1) segSizeMax)
            // NOTE: Publish linkage before the element becomes observable via count.
            t.Next <- newSeg
            tail <- newSeg
            // NOTE: Write into the new tail
            newSeg.Items[0] <- item
            tailIndex <- 1
        else
            t.Items[idx] <- item
            tailIndex <- idx + 1

        // NOTE: Publish new count after the element write (happens-before for reader).
        // NOTE: A volatile read is not required because SPSC guarantees a single writer.
        writeCount (count + 1)

    member __.Count: int = readCount ()

    // NOTE: for F# pattern enumeration optimization (zero allocation via struct enumerator).
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member __.GetEnumerator() =
        // NOTE: Snapshot the count once; traverse segments accordingly.
        let snapshotCount = readCount ()
        new SpscSegmentEnumerator<'T>(head, snapshotCount)

    interface IReadOnlyCollection<'T> with
        member __.Count = readCount ()

    interface IEnumerable<'T> with
        member __.GetEnumerator() : IEnumerator<'T> = __.GetEnumerator() :> IEnumerator<'T>

    interface IEnumerable with
        member __.GetEnumerator() : IEnumerator = __.GetEnumerator() :> IEnumerator
