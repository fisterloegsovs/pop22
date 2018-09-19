let N = 116
let mutable n = N
let mutable str = ""
while n > 0 do
  let rest = n % 2
  n <- n / 2
  if rest > 0 then
    str <- "1"+str
  else
    str <- "0"+str
printfn "%d_10 = %s_2" N str
