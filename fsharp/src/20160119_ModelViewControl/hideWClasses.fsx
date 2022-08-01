/// Demonstrate hiding using classes

type ITicker =
    abstract member tick : unit -> int

let generateTicker () =
    let mutable count = 0
    {new ITicker with
        member x.tick () = count <- count + 1; count}

let A = generateTicker ()
let B = generateTicker ()

printfn "tickerA = %d" (A.tick ())
printfn "tickerA = %d" (A.tick ())
printfn "tickerB = %d" (B.tick ())
printfn "tickerA = %d" (A.tick ())
