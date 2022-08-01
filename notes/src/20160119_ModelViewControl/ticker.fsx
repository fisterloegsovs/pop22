/// Generate 2 tickers and tick a couple of times
/// How to compile:
///   fsharpc ticker.fs ticker.fsx

let A = MyTicker.generateTicker ()
let B = MyTicker.generateTicker ()

printfn "tickerA = %d" (A.tick ())
printfn "tickerA = %d" (A.tick ())
printfn "tickerB = %d" (B.tick ())
printfn "tickerA = %d" (A.tick ())
