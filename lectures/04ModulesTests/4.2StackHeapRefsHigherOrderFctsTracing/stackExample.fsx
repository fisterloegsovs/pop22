let f x = x*x
let g x =
  let a = -1.0/2.0
  exp (a * f x)
printfn "%g" (g 2.0)
