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

( // Interactively draw 36 spokes, centered in the middle, and where the arrow keys control an initial angular offset

  /// <summary>Create a canvas with 36 spokes, centered in the middle, and with an inital angular offset</summary>
  /// <param w>the width of the resulting canvas</param>
  /// <param h>the height of the resulting canvas</param>
  /// <param s>the angular offset of all spokesr</param>
  /// <returns>a canvas with spokes</returns>
  let draw (w: int) (h: int) =
    let x = ((float w)/2.0, (float h)/2.0)
    let v = ((float w)/3.0,0.0)
    let C = create w h
    setVector C black v x
    C

  let v = (2.72,3.14)
  printfn "%A -> %A" v (toInt v)

  // Enter the interactive canvas loop
  let C = draw 600 600
  show C "Horizontal Line"
)