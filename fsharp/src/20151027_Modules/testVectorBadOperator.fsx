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

/// Opening the Vector module imports all internal definitions from the module into the current namespace, so beware!
open Vector

/// Make two vectors, add them, extract the resulting coordinates and print to screen.
let a = make(1.0, -2.0)
let b = make(3.0, 4.0)
let c = a + b
let (c1, c2) = coord c
printfn "a +. b = (%f, %f)" c1 c2

/// Compile error! Module has overloaded the operator, so now it only works for Vector types.
let w = 3.0 + 4.0
printfn "3.0 + 4.0 = %f" w
