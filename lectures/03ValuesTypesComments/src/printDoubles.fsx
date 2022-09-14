let printDoubles n =
  let mutable i = 1
  while i <= n do
    printf "%d " (2*i)
    i <- i + 1
  printfn ""

let n = 3
printDoubles n
