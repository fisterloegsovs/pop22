let fib (i : int) : int =
    if i < 1 then
        0
    else
        let mutable prevPrev = 0
        let mutable prev = 1
        let mutable curr = 1
        for n = 2 to i do
            curr <- prevPrev + prev
            prevPrev <- prev
            prev <- curr
        prev
            
printfn "fib 0 = %d" (fib 0)
printfn "fib 1 = %d" (fib 1)
printfn "fib 2 = %d" (fib 2)
printfn "fib 6 = %d" (fib 6)
