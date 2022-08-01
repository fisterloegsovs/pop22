// Demonstration of mutable records

type counter = {mutable value : int}

let incr (count:counter) =
  count.value <- count.value + 1
  count.value
  
let makeCounter () =
  let count = { value=0 }
  fun () -> incr count

let tic = makeCounter ()
printfn "%d" (tic ())
printfn "%d" (tic ())
