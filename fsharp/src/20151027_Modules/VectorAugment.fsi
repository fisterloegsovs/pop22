module Vector
/// A demonstration of defining a module from H & R, Functional Programming Using F#.
///
/// How to compile:
/// <code>
/// fsharpc --doc:VectorAugment.xml -a VectorAugment.fsi VectorAugment.fs
/// fsharpc --doc:testVectorAugment.xml -r VectorAugment.dll testVectorAugment.fsx
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/10/27

/// A 2 dimensional vector type, whose elements are floats.
[<Sealed>]
type Vector =
/// Vector negation
  static member ( ~- ) : Vector -> Vector
/// Vector addition
  static member ( + ) : Vector * Vector -> Vector
/// Vector subtraction
  static member ( - ) : Vector * Vector -> Vector
/// scalar-Vector multiplication
  static member ( * ) : float * Vector -> Vector
/// Vector dot product
  static member ( * ) : Vector * Vector -> float
/// Length of vector
val make : float * float -> Vector
/// Make vector
val coord: Vector -> float * float
/// Get coordinates
val norm : Vector -> float
