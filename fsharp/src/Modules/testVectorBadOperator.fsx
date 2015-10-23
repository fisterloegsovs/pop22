open Vector
let a = make(1.0, -2.0)
let b = make(3.0, 4.0)
let c = a + b
let (c1, c2) = coord c
printfn "a +. b = (%f, %f)" c1 c2

let w = 3.0 + 4.0
printfn "3.0 + 4.0 = %f" w
