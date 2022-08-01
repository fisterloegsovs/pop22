module Vector
/// A demonstration of defining a module from H & R, Functional Programming Using F#. Demonstrates namespace polution, and testVectorBadOperator.fsx won't compile!
///
/// How to compile:
/// <code>
/// fsharpc --doc:VectorBadOperator.xml -a VectorBadOperator.fsi VectorBadOperator.fs
/// fsharpc --doc:testVectorBadOperator.xml -r VectorBadOperator.dll testVectorBadOperator.fsx
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/10/27

/// The implementation of the Vector type. See interface file for detailed comments on its use.
type Vector = V of float * float
let ( + ) (V(x1,y1)) (V(x2,y2)) = V(x1+x2,y1+y2)
let make (x,y) = V(x,y)
let coord (V(x,y)) = (x,y)
