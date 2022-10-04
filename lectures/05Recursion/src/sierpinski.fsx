#r "nuget:DIKU.Canvas, 1.0.1"
open Canvas

type vec = float*float // a float vector

/// convert a float vector to a tuple of integers
let toInt ((v1,v2):vec) : int*int = (int v1,int v2)

/// draw the (a,b,c) triangle with color col and on canvas C
let setTriangle (a:vec) (b:vec) (c:vec) (col:color) (C:canvas): unit =
  let (v1, v2, v3) = (toInt a, toInt b, toInt c)
  setLine C col v1 v2
  setLine C col v2 v3
  setLine C col v3 v1

/// Return the average of two vectors
let avg ((u1,u2):vec) ((v1,v2):vec): vec = ((u1+v1)/2.0, (u2+v2)/2.0)

/// Draw the sierpinski triangle with color col and on canvas C
/// When n=0 then the triangle (a,b,c) is drawn, when n > 0 the
/// triangle is subdivided into 3 sierpinski triangles
let rec sierpinski (C: canvas) (col: color) (a:vec) (b:vec) (c:vec) (n:uint): unit =
  match n with
    0u -> setTriangle a b c col C
    | _ ->
      let ab = avg a b
      let bc = avg b c
      let ac = avg a c
      sierpinski C col a ab ac (n-1u)
      sierpinski C col ab b bc (n-1u)
      sierpinski C col ac bc c (n-1u)

let w = 600.0
let C = create (int w) (int w)
sierpinski C black (0.0,0.0) (w/2.0,w-1.0) (w-1.0, 0.0) 10u
show C "Sierpinski"