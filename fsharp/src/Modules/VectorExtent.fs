module Vector
type Vector = V of float * float
let make (x,y) = V(x,y)
let norm (V(x,y)) = sqrt(x*x+y*y)
type Vector with
  static member ( ~+. ) (V(x1,y1)) = norm(V(x1,y1))
