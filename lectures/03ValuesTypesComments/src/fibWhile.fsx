let fib n =
  if n < 1 then 0
  else
    let mutable prevPrev = 0
    let mutable prev = 1
    let mutable i = 2
    while i <= n do
      let curr = prev + prevPrev
      prevPrev <- prev
      prev <- curr
      i <- i + 1
    prev

for i = 0 to 45 do
  printfn "fib(%d) = %d" i (fib i)
