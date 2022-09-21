let arr = [| 3.0; 4.0 |]
printfn "%A" arr
for i = 0 to 1 do
  arr.[i] <- float(i)
  printfn "%A" arr
printfn "%A" arr
