let dbl x = 2 * x
let apply dbl x = dbl x
printfn "%A, %A" (dbl 3) (apply dbl 3)
