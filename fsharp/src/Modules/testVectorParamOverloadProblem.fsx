let a = Vector.make(1.0, -2.0)
let b = Vector.make(3.0, 4.0)
let c = a +. b;
let (c1,c2) = Vector.coord(c)
printfn "a +. b = (%f, %f)" c1 c2
