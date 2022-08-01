// Variant of demonstration of reference variables

let incr (count: int ref) =
  count := !count + 1;
  !count
  
let makeCounter () =
  ref 0

let value (count: int ref) =
  !count

let counter = makeCounter ()
printfn "Current value: %d" (value counter)
printfn "After a tic: %d" (incr counter)
printfn "Current value: %d" (value counter)
printfn "After a tic: %d" (incr counter)
