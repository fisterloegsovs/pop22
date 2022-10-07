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

type triangle = vec*vec*vec // 3 points making a a triangle

/// Return a list of triangles by subdividing (a,b,c)
let subdivide ((a,b,c):triangle): triangle list =
  let ab = avg a b
  let bc = avg b c
  let ac = avg a c
  [(a, ab, ac); (ab, b, bc); (ac, bc, c)]

/// Draw the sierpinski triangle with color col and on canvas C
/// When n=0 all triangles in the list lst is drawn, when n > 0 each
/// triangle in lst is subdivided into 3 sierpinski triangles
let rec sierpinski (C: canvas) (col: color) (n:uint) (lst: triangle list ): unit =
  match n with
    0u -> List.iter (fun (a, b, c) -> setTriangle a b c col C) lst
    | _ ->
      lst
      |> List.map subdivide
      |> List.concat
      |> sierpinski C col (n-1u)

let w = 600.0
let C = create (int w) (int w)
sierpinski C black 10u [((0.0,0.0),((w-1.0)/2.0,w-1.0),(w-1.0, 0.0))] 
show C "Sierpinski"