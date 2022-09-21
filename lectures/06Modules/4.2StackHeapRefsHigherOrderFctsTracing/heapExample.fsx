let g a x =
  a := -1.0/2.0
  exp (!a * x * x)
let a = ref -1.0
printfn "%g" (g a 2.0)
printfn "%g" !a