let fib n =
  match n with
    0 | 1 ->
      1
    | _ ->
      let mutable prevPrev = 0
      let mutable prev = 1
    
      for i = 2 to n do
        let curr = prev + prevPrev
        prevPrev <- prev
        prev <- curr
      prev

for i = 0 to 45 do
  printfn "fib(%d) = %d" i (fib i)
