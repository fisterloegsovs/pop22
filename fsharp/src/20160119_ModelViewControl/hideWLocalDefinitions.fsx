/// Demonstrate hiding with local definition. Note that there is only one instance of generateTicker!

let generateTicker =
    let mutable count = 0
    (fun () -> count <- count + 1; count)

let tickerA = generateTicker
let tickerB = generateTicker

printfn "tickerA = %d" (tickerA ())
printfn "tickerA = %d" (tickerA ())
printfn "tickerB = %d" (tickerB ())
printfn "tickerA = %d" (tickerA ())
