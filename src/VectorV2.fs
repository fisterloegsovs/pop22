module Vector
type Vector =
  | V of float * float
  static member ( ~- ) (V(x1,y1)) = V(-x1,-y1)
  static member ( + ) (V(x1,y1), V(x2,y2)) = V(x1+x2,y1+y2)
  static member ( - ) (V(x1,y1), V(x2,y2)) = V(x1-x2,y1-y2)
  static member ( * ) (a, V(x1,y1)) = V(a*x1,a*y1)
  static member ( * ) (V(x1,y1), V(x2,y2)) = x1*x2+y1*y2
let make (x,y) = V(x,y)
let coord (V(x,y)) = (x,y)
let norm (V(x,y)) = sqrt(x*x+y*y)
