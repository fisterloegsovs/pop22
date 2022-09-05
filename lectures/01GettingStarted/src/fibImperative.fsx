let fib n =
  match n with
    0 -> 0
    | 1 -> 1
    | _ ->
      let mutable prevPrev = 0
      let mutable prev = 1
      let mutable i = 2;
    
      while i <= n do
        let curr = prev + prevPrev
        prevPrev <- prev
        prev <- curr
        i <- i + 1
      prev

let mutable i = 0
while i <= 45 do
  printfn "fib(%d) = %d" i (fib i)
  i <- i + 1