let rec sum s n = 
  if n = 0 then s 
  else sum (n + s) (n - 1)
printfn "%A" (sum 0 1000000)                                         
