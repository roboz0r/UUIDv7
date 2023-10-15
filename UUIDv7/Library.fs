namespace UUIDv7

// https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-01.html#name-uuidv7-layout-and-bit-order
// UUIDv7 has microsecond resolution, 64-bit timestamp, 16-bit sequence number, and 64-bit node ID.

open System
open System.Diagnostics
open System.Security.Cryptography
open System.Text

module internal HexConverter =
    let hexCharsUpper = "0123456789ABCDEF"
    let hexCharsLower = "0123456789abcdef"

    [<Literal>]
    let Mask = 0xF000_0000_0000_0000UL

    let private guidBuilder = StringBuilder(36)

    let private formatGuidB (hexChars: string) (high: uint64) (low: uint64) (sb: StringBuilder) =

        let rec loopHigh i x =
            match i with
            | 0 -> ()
            | _ ->
                sb.Append(hexChars.[int ((x &&& Mask) >>> 60)])
                |> ignore

                if i = 9 || i = 5 || i = 1 then
                    sb.Append('-') |> ignore

                loopHigh (i - 1) (x <<< 4)

        let rec loopLow i x =
            match i with
            | 0 -> ()
            | _ ->
                sb.Append(hexChars.[int ((x &&& Mask) >>> 60)])
                |> ignore

                if i = 13 then sb.Append('-') |> ignore
                loopLow (i - 1) (x <<< 4)

        loopHigh 16 high
        loopLow 16 low

    let formatGuid uppercase high low =
        lock guidBuilder (fun () ->
            let hexChars =
                if uppercase then
                    hexCharsUpper
                else
                    hexCharsLower

            guidBuilder.Clear() |> ignore
            formatGuidB hexChars high low guidBuilder
            guidBuilder.ToString())

open System.Runtime.InteropServices

[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type UUIDv7(high: uint64, low: uint64) =
    member _.High = high
    member _.Low = low

    member _.Version = (high <<< 48) >>> 60

    member _.Variant = low >>> 62

    member _.ToUnixTimeSeconds() = int64 (high >>> 28)

    member _.FullTimestamp =
        let low12 = (high &&& 0xFFFUL)
        let seconds = TimeSpan.FromSeconds(float (high >>> 28))

        let usec =
            let x = (((high >>> 16) &&& 0xFFFUL) <<< 12) ||| low12
            TimeSpan.FromMilliseconds((float x) / 1000.0)

        DateTimeOffset.UnixEpoch + seconds + usec

    override _.ToString() = HexConverter.formatGuid false high low

module internal UUIDv7Helpers =

    // Figure 4: UUIDv7 Field and Bit Layout - Encoding Example (Microsecond Precision)
    //  0                   1                   2                   3
    //  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    // +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    // |                            unixts                             |
    // +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    // |unixts |         usec          |  ver  |         usec          |
    // +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    // |var|             seq           |            rand               |
    // +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    // |                             rand                              |
    // +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+


    [<Literal>]
    let UuidVer = 0x7000UL

    [<Literal>]
    let TSTopMask = 0xFFFFFFFFFFFF0000UL

    [<Literal>]
    let TSBottomMask = 0xFFFUL

    [<Literal>]
    let SeqInit = 0b1000_0000_0000_0000UL

    [<Literal>]
    let SeqMax = 0b1011_1111_1111_1111UL

    [<Literal>]
    let RandMask = 0xFFFF_FFFF_FFFFUL

    type LowBits(rng: RandomNumberGenerator) =
        let mutable i = SeqInit
        let gate = obj ()

        let mutable lastTs = 0UL
        let rand = Array.zeroCreate<uint64> 256
        let mutable randI = 0uy

        let fromSeq seqId =
            if randI = 0uy then
                rng.GetBytes(MemoryMarshal.AsBytes(Span(rand)))

            let mutable randBits = rand.[int randI] &&& RandMask

            while randBits = 0UL do
                randI <- randI + 1uy
                randBits <- rand.[int randI] &&& RandMask

            let x = (seqId <<< 48) ||| randBits

            randI <- randI + 1uy
            x

        new() = LowBits(RandomNumberGenerator.Create())

        member _.TryNext(timeStamp) =
            lock gate (fun () ->
                if timeStamp = lastTs then
                    match i with
                    | SeqMax -> ValueNone
                    | x ->
                        let x1 = x + 1UL
                        i <- x1
                        ValueSome(fromSeq x1)
                elif timeStamp > lastTs then
                    lastTs <- timeStamp
                    i <- SeqInit
                    ValueSome(fromSeq SeqInit)
                else
                    ValueNone

            )

    type HighBits() =

        let usFreq =
            match Stopwatch.Frequency / 1_000_000L with
            | 0L ->
                raise (
                    PlatformNotSupportedException(
                        "Stopwatch.Frequency < 1_000_000L. High resolution timer is required."
                    )
                )
            | usFreq -> usFreq

        let stopwatch = Stopwatch.StartNew()
        let mutable unixTime = 0UL
        let mutable usTime = 0UL

        let rec updateTime () =
            let now = DateTimeOffset.UtcNow
            stopwatch.Restart()
            let oldUnixTime = unixTime
            unixTime <- uint64 (now.ToUnixTimeSeconds()) <<< 28

            if oldUnixTime = unixTime then
                let oldUsNow = usTime
                usTime <- (uint64 now.Millisecond) * 1000UL

                if oldUsNow >= usTime then
                    // Ensure that time is monotonically increasing
                    // Millisecond roundoff detected
                    updateTime ()
            else
                usTime <- (uint64 now.Millisecond) * 1000UL

        do updateTime ()

        let rec getMicroseconds looping =
            let ticks = stopwatch.ElapsedTicks

            if looping || ticks > 500_000L then
                let oldUnixTime = unixTime
                let uSecs = usTime + (uint64 (ticks / usFreq))
                updateTime ()
                let ticks2 = stopwatch.ElapsedTicks
                let uSecs2 = usTime + (uint64 (ticks2 / usFreq))

                if oldUnixTime = unixTime && uSecs2 < uSecs then
                    // Ensure that time is monotonically increasing
                    // Tick drift detected.
                    getMicroseconds true
                else
                    uSecs2
            else
                usTime + (uint64 (ticks / usFreq))

        member _.Next() =
            lock stopwatch (fun () ->
                let uSecs = getMicroseconds false

                let bottom12 = uSecs &&& TSBottomMask
                let ts = (uSecs <<< 4) + unixTime
                let top48 = ts &&& TSTopMask
                top48 ||| UuidVer ||| bottom12

            )


    let mutable private timestamp = Unchecked.defaultof<_>
    let mutable private clock = Unchecked.defaultof<_>

    let rec nextUuid () =
        if obj.ReferenceEquals(timestamp, null) then
            timestamp <- HighBits()
            clock <- LowBits()

        let high = timestamp.Next()

        match clock.TryNext high with
        | ValueSome low -> UUIDv7(high, low)
        | ValueNone -> nextUuid ()

    let toGuid (x: UUIDv7) =
        // UUIDv7 and Guid have different byte order
        let span = NativePtr.stackallocSpan<uint64> 2
        span[0] <- x.Low
        span[1] <- x.High
        let span = MemoryMarshal.AsBytes(span)
        span.Reverse()
        span.Slice(0, 4).Reverse()
        span.Slice(4, 2).Reverse()
        span.Slice(6, 2).Reverse()
        Guid(span)

type UUIDv7 with
    static member New() = UUIDv7Helpers.nextUuid ()

    member this.ToGuid() = UUIDv7Helpers.toGuid this
