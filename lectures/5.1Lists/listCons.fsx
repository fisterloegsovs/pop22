let mutable lst = []
for i = 1 to 10 do
  lst <- i :: lst
printfn "%A" lst
