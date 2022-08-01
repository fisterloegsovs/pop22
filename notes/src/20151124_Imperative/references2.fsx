// Demonstration of reference variables

let incr (count: int ref) =
  count := !count + 1;
  !count
  
let makeCounter () =
  let count = ref 0
  fun () -> incr count

let tic = makeCounter ()
printfn "%d" (tic ())
printfn "%d" (tic ())
