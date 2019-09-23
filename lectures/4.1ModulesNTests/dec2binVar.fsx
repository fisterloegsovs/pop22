let N = 116
let mutable n = N
let mutable str = ""
while n > 0 do
 str <- (if n % 2 > 0 then "1" else "0") + str
 n <- n / 2
printfn "%d_10 = %s_2" N str
