let a = 3
let b = 4
let f a b x = a * x + b
let mutable x = 0
while x < 6 do
  printfn "%A * %A + %A = %A" a x b (f a b x)
  x <- x + 1
