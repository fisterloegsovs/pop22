#r "nuget:DIKU.Canvas, 1.0"
open Canvas

type vec = float*float // A 2-dimensional vector

/// <summary>Add two 2-dimensional vectors</summary>
/// <param x1>x-coordinate of the first vector</param>
/// <param y1>y-coordinate of the first vector</param>
/// <param x2>x-coordinate of the second vector</param>
/// <param y2>y-coordinate of the second vector</param>
/// <returns>The vector sum</returns>
let add (v1: vec) (v2: vec) : vec =
  let (x1,y1) = v1
  let (x2,y2) = v2
  (x1+x2,y1+y2)

/// <summary>Multiplication of a vector with a scalar</summary>
/// <param x>x-coordinate of the vector</param>
/// <param y>y-coordinate of the vector</param>
/// <param a>a multiplicative scalar</param>
/// <returns>The scalar-vector product</returns>
let mul (v: vec) (a: float) : vec =
  let (x,y) = v
  (a*x,a*y)

/// <summary>Dot two 2-dimensional vectors</summary>
/// <param x1>x-coordinate of the first vector</param>
/// <param y1>y-coordinate of the first vector</param>
/// <param x2>x-coordinate of the second vector</param>
/// <param y2>y-coordinate of the second vector</param>
/// <returns>The vector dot-product</returns>
let dot (v1: vec) (v2: vec) : float =
  let (x1,y1) = v1
  let (x2,y2) = v2
  x1*x2+y1*y2

/// <summary>Rotation of a vector around its tail</summary>
/// <param x>x-coordinate of the vector</param>
/// <param y>y-coordinate of the vector</param>
/// <param a>an angle in the range 0..2 pi</param>
/// <returns>The rotated vector</returns>
let rot (v: vec) (a: float) : vec =
  let (x,y) = v
  (x*cos a - y*sin a, x*sin a + y*cos a)

/// <summary>Conversion of a vector wit float components to int</summary>
/// <param x>x-coordinate of the vector</param>
/// <param y>y-coordinate of the vector</param>
/// <returns>The vector casted to int</returns>
let toInt (v: vec) : int*int =
  let (x,y) = v
  (int x, int y)

/// <summary>Update the canvas with a vector drawn as a line from p to p+v</summary>
/// <param C>a canvas</param>
/// <param col>the color to be used</param>
/// <param v>a vector</param>
/// <param p>a vector giving the position of the tail to be drawn</param>
/// <returns>The canvas is updated as a side-effect</returns>
let setVector (C: canvas) (col: color) (v: vec) (p: vec) : unit = 
  setLine C col (toInt p) (toInt (add v p))


( // Testing the basic operations
  let v = (1.0,2.3)
  let w = (-3.5,2.2)
  let a = 4.0

  printfn "v = %A, w = %A, a = %A" v w a
  printfn "v + w = %A" (add v w)
  printfn "av = %A" (mul v a)
  printfn "R (1,0) = %A" (rot (1.0,0.0) (System.Math.PI/2.0))
)

type state = float // The spoke-angle offset will be communicated between draw and react
( // Interactively draw 36 spokes, centered in the middle, and where the arrow keys control an initial angular offset

  /// <summary>Create a canvas with 36 spokes, centered in the middle, and with an inital angular offset</summary>
  /// <param w>the width of the resulting canvas</param>
  /// <param h>the height of the resulting canvas</param>
  /// <param s>the angular offset of all spokesr</param>
  /// <returns>a canvas with spokes</returns>
  let draw (w: int) (h: int) (s:state) =
    let rec fan C col u a n =
      match n with 
        0 -> ()
        | _ ->
          let v = rot u (s+a*float n)
          setVector C col v (300.0,300.0)
          fan C col u a (n-1)
    let n = 36
    let da = 2.0*System.Math.PI/float n
    let C = create w h
    let u = (0.0,float (h/2))
    fan C black u da n
    C

  /// <summary>Convert a key-input to an offset</summary>
  /// <param s>the present angular offset of all spokesr</param>
  /// <param k>the pressed key</param>
  /// <returns>an updated offset</returns>
  let react (s:state) (k:Canvas.key) : state option =
      match getKey k with
          | LeftArrow -> Some (s-0.01)
          | RightArrow -> Some (s+0.01)
          | _ -> None

  // Enter the interactive canvas loop
  do runApp "ColorTest" 600 600 draw react 0.0
)