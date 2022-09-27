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

// Testing the basic operations
let v = (1.0,2.3)
let w = (-3.5,2.2)
let a = 4.0

printfn "v = %A, w = %A, a = %A" v w a
printfn "v + w = %A" (add v w)
printfn "av = %A" (mul v a)
printfn "R (1,0) = %A" (rot (1.0,0.0) (System.Math.PI/2.0))