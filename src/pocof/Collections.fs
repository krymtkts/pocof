namespace Pocof

open System
open System.Collections
open System.Collections.Generic
open System.Threading

[<Sealed>]
type SpscSegment<'T>(capacity: int) =
    let items: 'T array = Array.zeroCreate capacity
    let mutable next: SpscSegment<'T> | null = null
    member __.Items = items
    member __.Capacity = items.Length

    member __.Next
        with get () = Volatile.Read(&next)
        and set (v) = Volatile.Write(&next, v)

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
    member __.MoveNext() =
        if __.remaining <= 0 then
            false
        else
            if __.idx >= __.cap then
                let next = __.seg.Next

                if isNull next then
                    __.remaining <- 0
                else
                    __.seg <- next
                    let items = next.Items
                    __.items <- items
                    __.cap <- items.Length
                    __.idx <- 0

            if __.remaining > 0 then
                let i = __.idx
                __.current <- __.items[i]
                __.idx <- i + 1
                __.remaining <- __.remaining - 1
                true
            else
                false

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
    let segSizeMax = 8192

    // NOTE: Head (first) and tail (current write) segments.
    let head = SpscSegment<'T>(segSize)
    let mutable tail = head
    let mutable tailIndex = 0 // next write index within tail

    // NOTE: Published element count (used for snapshot and IReadOnlyCollection.Count).
    let mutable count = 0

    // NOTE: Volatile helpers for count.
    let readCount () = Volatile.Read(&count)
    let writeCount (v: int) = Volatile.Write(&count, v)

    member __.Add(item: 'T) : unit =
        // NOTE: Acquire local view of the current tail segment.
        let t = tail
        let idx = tailIndex

        if idx >= t.Capacity then
            // NOTE: Current segment is full: create a new one, link it, and advance tail.
            let newSeg = SpscSegment<'T>(min (t.Capacity <<< 1) segSizeMax)
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
