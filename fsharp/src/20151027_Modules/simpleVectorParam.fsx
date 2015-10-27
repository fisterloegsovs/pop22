/// A demonstration of type parameterisation.
///
/// How to compile:
/// <code>
/// fsharpc --doc:simpleVectorParam.xml simpleVectorParam.fsx
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/10/27

/// Define a parameterised type.
type Vector<'a> =
  | V of 'a * 'a
  static member inline ( +. ) (V(x1,y1), V(x2,y2)) = V(x1+x2, y1+y2)
let coord (V(x, y)) = (x, y)

/// Define 2 vectors, add them with the overloaded inline operator, extract coordinates and print the result
let a = V(1.0, -2.0)
let b = V(3.0, 4.0)
let c = a +. b;
let (c1,c2) = coord(c);
printfn "a +. b = (%f, %f)" c1 c2
