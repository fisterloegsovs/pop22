/// A demonstration of the problem with parametrized type and build in operators.
///
/// How to compile:
/// <code>
/// fsharpc --doc:VectorParam.xml -a VectorParam.fsi VectorParam.fs
/// fsharpc --doc:testVectorParam.xml -r VectorParam.dll testVectorParam.fsx
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/10/27

/// Make a vector and calculate its norm.
let a = Vector.make(1.0, -2.0) : Vector.Vector<float>
let b = Vector.make(3.0, 4.0) : Vector.Vector<float>
let c = Vector.add a b;
let (c1,c2) = Vector.coord(c)
printfn "a + b = (%f, %f)" c1 c2
