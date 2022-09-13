let mutable sum = 1
let mutable i = 1
while i < 3 do
  sum <- sum + i
  i <- i + 1
printfn "i = %A, sum = %A" i sum