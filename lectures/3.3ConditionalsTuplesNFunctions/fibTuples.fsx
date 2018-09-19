let mutable pair = (1,1)
for i = 3 to 5 do
  pair <- (snd pair, fst pair + snd pair)
printfn "%d" (snd pair)
