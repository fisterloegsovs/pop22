let lst = [ 3.0; 4.0 ]
printfn "%A" lst
for i = 0 to 1 do
  let lst = [0 .. i]
  printfn "%A" lst
printfn "%A" lst
