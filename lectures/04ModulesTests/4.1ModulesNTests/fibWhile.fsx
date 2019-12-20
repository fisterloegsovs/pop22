let mutable m = 1
let mutable n = 1
let mutable i = 3
while i <= 5 do
  let p = m + n
  m <- n
  n <- p
  i <- i + 1;
printfn "%d: %d" i n
