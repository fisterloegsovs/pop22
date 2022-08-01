module Vector
/// A demonstration of defining a module from H & R, Functional Programming Using F#. Note: Bad style, better use augmented types.
///
/// How to compile:
/// <code>
/// fsharpc --doc:Vector.xml -a Vector.fsi Vector.fs
/// fsharpc --doc:testVector.xml -r Vector.dll testVector.fsx
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/10/27

/// The implementation of the Vector type. See interface file for detailed comments on its use.
type Vector = V of float * float
let ( ~-. ) (V(x1,y1)) = V(-x1,-y1)
let ( +. ) (V(x1,y1)) (V(x2,y2)) = V(x1+x2,y1+y2)
let ( -. ) (V(x1,y1)) (V(x2,y2)) = V(x1-x2,y1-y2)
let ( *. ) a (V(x1,y1)) = V(a*x1,a*y1)
let ( &. ) (V(x1,y1)) (V(x2,y2)) = x1*x2+y1*y2
let norm (V(x,y)) = sqrt(x*x+y*y)
let make (x,y) = V(x,y)
let coord (V(x,y)) = (x,y)
