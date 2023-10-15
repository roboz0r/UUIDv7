module UUIDv7Tests

open System
open Expecto

open UUIDv7

let checkInvariants (uuid: UUIDv7) =
    let guid = uuid.ToGuid()
    let uuidString = uuid.ToString()
    let guidString = guid.ToString("D")

    "UUIDv7 should be valid Guid"
    |> Expect.equal uuidString guidString

    "UUIDv7 should have valid Variant"
    |> Expect.equal uuid.Variant 2UL

    "UUIDv7 should have valid Version"
    |> Expect.equal uuid.Version 7UL

    "UUID7 Random bits are non-zero"
    |> Expect.isGreaterThan (uuid.Low &&& 0xFFFF_FFFF_FFFFUL) 0UL


[<Tests>]
let tests =
    testList
        "UUIDv7"
        [
            test "UUIDv7s are generated in order" {
                let rng = Random(123)

                for i in 1..100 do
                    let count = rng.Next(2, 10_000)
                    let uuids = Array.init count (fun _ -> UUIDv7.New())
                    let sorted = uuids |> Array.sort
                    Expect.equal sorted uuids "UUIDv7 should be generated in order"
                    for i in 0..count-2 do
                        Expect.isLessThan uuids.[i] uuids.[i+1] "UUIDv7 should be generated in order"

                    for uuid in uuids do
                        checkInvariants uuid
            }

            test "UUIDv7 contains unix seconds in first bits" {
                let rng = Random(123)

                for i in 1..100 do
                    let uuid = UUIDv7.New()
                    let unixTime = uuid.ToUnixTimeSeconds()
                    let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()

                    Expect.isLessThanOrEqual
                        (abs (unixTime - now))
                        1L
                        "UUIDv7 should contain unix seconds in first bits"

                    Threading.Thread.Sleep(rng.Next(1, 100))
                    checkInvariants uuid
            }

            test "UUID7 HighBits are always equal or increasing" {
                let highBits = UUIDv7Helpers.HighBits()

                for i in 1..10_000_000 do
                    let highBits0 = highBits.Next()
                    let highBits1 = highBits.Next()
                    let highBits2 = highBits.Next()
                    let highBits3 = highBits.Next()
                    let highBits4 = highBits.Next()

                    Expect.isLessThanOrEqual highBits0 highBits1 "UUID7 HighBits is always equal or increasing 1"
                    Expect.isLessThanOrEqual highBits1 highBits2 "UUID7 HighBits is always equal or increasing 2"
                    Expect.isLessThanOrEqual highBits2 highBits3 "UUID7 HighBits is always equal or increasing 3"
                    Expect.isLessThanOrEqual highBits3 highBits4 "UUID7 HighBits is always equal or increasing 4"
            }

            test "UUID7 LowBits are always increasing" {
                let highBits = UUIDv7Helpers.HighBits()
                let lowBits = UUIDv7Helpers.LowBits()
                let mutable highBits0 = highBits.Next()

                for i in 1..10_000_000 do
                    let lowBits0 = lowBits.TryNext(highBits0)
                    let lowBits1 = lowBits.TryNext(highBits0)

                    match lowBits0, lowBits1 with
                    | ValueSome lowBits0, ValueSome lowBits1 ->
                        Expect.isLessThan lowBits0 lowBits1 "UUID7 LowBits is always increasing"
                    | _ -> highBits0 <- highBits.Next()
            }
        ]



[<EntryPoint>]
let main args = runTestsInAssemblyWithCLIArgs [] args
