type Vector<'a> =
  | V of 'a * 'a
  static member ( +. ) (V(x1,y1), V(x2,y2)) = V(x1+x2, y1+y2)
let make (x, y) = V(x, y)
let coord (V(x, y)) = (x, y)

let a = make(1.0, -2.0)
let b = make(3.0, 4.0)
let c = a +. b;
let (c1,c2) = coord(c)
printfn "a +. b = (%f, %f)" c1 c2
