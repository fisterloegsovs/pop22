let mutable m = 1
let mutable n = 1
for i = 3 to 5 do
  let p = m + n
  m <- n
  n <- p
printfn "%d: %d" i n
