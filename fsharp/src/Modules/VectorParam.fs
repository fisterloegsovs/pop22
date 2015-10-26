module Vector
type Vector<'t> = V of 't * 't
type Vector with
  static member inline ( +. ) (V(x1, y1), V(x2, y2)) = V(x1 + x2, y1 + y2)
let make (x, y) = V(x, y)
let coord (V(x, y)) = (x, y)
