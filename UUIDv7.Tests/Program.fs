open System
open System.Diagnostics
open BenchmarkDotNet.Attributes

open UUIDv7
open BenchmarkDotNet.Running

[<MemoryDiagnoser>]
type Benchmark1() =

    [<Benchmark>]
    member __.Test1k() =
        let mutable uuid = UUIDv7()

        for i in 0..1_000 do
            uuid <- UUIDv7.New()

        uuid

    [<Benchmark>]
    member __.Test1M() =
        let mutable uuid = UUIDv7.New()

        for i in 0..1_000_000 do
            uuid <- UUIDv7.New()

        uuid

    [<Benchmark>]
    member __.Test10M() =
        let mutable uuid = UUIDv7.New()

        for i in 0..10_000_000 do
            uuid <- UUIDv7.New()

        uuid

    [<Benchmark>]
    member __.Test1kToString() =
        let mutable s = ""

        for i in 0..1_000 do
            s <- UUIDv7.New().ToString()

        s

    [<Benchmark>]
    member __.Test1kGuid() =
        let mutable s = Guid.Empty

        for i in 0..1_000 do
            s <- Guid.NewGuid()

        s


[<EntryPoint>]
let main _ =
    let _ = BenchmarkRunner.Run<Benchmark1>()
    0
