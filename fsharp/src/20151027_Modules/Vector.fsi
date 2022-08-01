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

/// A 2 dimensional vector type, whose elements are floats.
type Vector
/// Vector negation
val ( ~-. ) : Vector -> Vector 
/// Vector addition
val ( +. ) : Vector -> Vector -> Vector
/// Vector subtraction
val ( -. ) : Vector -> Vector -> Vector
/// scalar-Vector multiplication
val ( *. ) : float -> Vector -> Vector
/// Vector dot product
val ( &. ) : Vector -> Vector -> float
/// Length of vector
val norm  : Vector -> float
/// Make vector
val make  : float * float -> Vector
/// Get coordinates
val coord : Vector -> float * float
