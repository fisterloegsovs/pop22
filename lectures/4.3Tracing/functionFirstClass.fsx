let mul x y = x * y
let factor = 2.0
let applyFactor fct x = 
  let a = fct factor x
  string a

printfn "%g" (mul 5.0 3.0)
printfn "%s" (applyFactor mul 3.0)
