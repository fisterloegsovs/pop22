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

/// A 2 dimensional vector type, whose elements are floats.
type Vector
/// Vector addition
val ( + ) : Vector -> Vector -> Vector // Vector addition
/// Make vector
val make  : float * float -> Vector // Make vector
/// Get coordinates
val coord : Vector -> float * float // Get coordinates
