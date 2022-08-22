type obj = Pnt | Circ of float | Rect of float * float

let rec area (obj:obj) : float =
  match obj with
    | Pnt -> 0.0
    | Circ r -> System.Math.PI * r * r
    | Rect (a,b) -> a * b

let c = Circ 5.0

let a = area c;;
