module Vector
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

/// The implementation of the Vector type. See interface file for detailed comments on its use.
type Vector<'t> = V of 't * 't
let make (x, y) = V(x, y)
let coord (V(x, y)) = (x, y)
let add (V(x1, y1)) (V(x2, y2)) = V(x1+x2, y1+y2)
