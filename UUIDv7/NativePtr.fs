module internal NativePtr

#nowarn "9"

open System
open Microsoft.FSharp.NativeInterop
let inline stackallocSpan<'T when 'T: unmanaged> (count: int) =
    let ptr = NativePtr.stackalloc<'T> count
    let span = Span<'T>(NativePtr.toVoidPtr ptr, count)
    span