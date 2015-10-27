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

/// A 2 dimensional vector type, whose elements are parameterised.
type Vector<'t>
/// Vector addition
val add : Vector<'t> -> Vector<'t> -> Vector<'t>
/// Make vector
val make : 't * 't -> Vector<'t>
/// Get coordinates
val coord : Vector<'t> -> 't * 't
