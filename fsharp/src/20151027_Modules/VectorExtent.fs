module Vector
/// A demonstration of defining a module from H & R, Functional Programming Using F#.
///
/// How to compile:
/// <code>
/// fsharpc --doc:VectorExtent.xml -a VectorExtent.fsi VectorExtent.fs
/// fsharpc --doc:testVectorExtent.xml -r VectorExtent.dll testVectorExtent.fsx
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/10/27

/// The implementation of the Vector type. See interface file for detailed comments on its use.
type Vector = V of float * float
let make (x,y) = V(x,y)
let norm (V(x,y)) = sqrt(x*x+y*y)
/// We use type with to continue the declare the Vector type using above methods. Choice of overload operator is limited
type Vector with
  static member ( !! ) (V(x1,y1)) = norm(V(x1,y1))
