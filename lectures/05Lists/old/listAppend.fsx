let mutable lst = []
for i = 1 to 10 do
  lst <- lst @ [i]
printfn "%A" lst
