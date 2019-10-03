let a = 3
let b = 4
let f a b x = a * x + b
for x = 0 to 5 do
  printfn "%A * %A + %A = %A" a x b (f a b x)
