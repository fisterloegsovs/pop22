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

/// Make a vector and calculate its norm.
let a = Vector.make(1.0, -2.0)
let b = !! a;
printfn "!! a = %f" b
