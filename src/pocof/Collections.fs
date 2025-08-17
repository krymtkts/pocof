namespace Pocof

open System.Collections
open System.Collections.Generic
open System.Threading

[<AllowNullLiteral; Sealed>]
type SpscSegment<'T>(capacity: int) =
    let items: 'T array = Array.zeroCreate capacity
    let mutable next: SpscSegment<'T> = null
    member _.Items = items
    member _.Capacity = items.Length

    member _.Next
        with get () = Volatile.Read(&next)
        and set (v) = Volatile.Write(&next, v)

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

    interface IReadOnlyCollection<'T> with
        member __.Count = readCount ()

    interface IEnumerable<'T> with
        member __.GetEnumerator() : IEnumerator<'T> =
            // NOTE: Snapshot the count once; traverse segments accordingly.
            let snapshotCount = readCount ()
            let mutable remaining = snapshotCount
            let mutable seg = head
            let mutable idx = 0

            (seq {
                while remaining > 0 do
                    if idx >= seg.Capacity then
                        seg <- seg.Next
                        idx <- 0

                    yield seg.Items[idx]
                    idx <- idx + 1
                    remaining <- remaining - 1
            })
                .GetEnumerator()

    interface IEnumerable with
        member __.GetEnumerator() : IEnumerator =
            (__ :> IEnumerable<'T>).GetEnumerator() :> IEnumerator
