let a = Vector.make(1.0, -2.0)
let (a1, a2) = Vector.coord a
printfn "a = (%f, %f)" a1 a2

let b = Vector.make(3.0, 4.0)
let (b1, b2) = Vector.coord b
printfn "b = (%f, %f)" b1 b2

let v = Vector.norm b
printfn "|b| = %f" v

let c = a + b
let (c1, c2) = Vector.coord c
printfn "a + b = (%f, %f)" c1 c2

let d = a - b
let (d1, d2) = Vector.coord d
printfn "a - b = (%f, %f)" d1 d2

let e = -a
let (e1, e2) = Vector.coord e
printfn "-a = (%f, %f)" e1 e2

let f = 3.0 * a
let (f1, f2) = Vector.coord f
printfn "3 * a = (%f, %f)" f1 f2

let w = a * b
printfn "a * b = %f" w
