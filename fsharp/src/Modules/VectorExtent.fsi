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

/// A 2 dimensional vector type, whose elements are floats.
[<Sealed>]
type Vector =
/// Vector norm
  static member ( !! ) : Vector -> float
/// Make vector
val make : float * float -> Vector
/// Make vector
val norm : Vector -> float
