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

/// Opening the Vector module imports all internal definitions from the module into the current namespace, so beware!
open Vector

/// Make a vector, extract its coordinates and display them on screen.
let a = make(1.0, -2.0)
let (a1, a2) = coord a
printfn "a = (%f, %f)" a1 a2

/// Calculate the Euclidean lenght of the vector and display on screen.
let v = norm a
printfn "|a| = %f" v

/// Negate a vector with overloaded operator from the module
let e = -.a
let (e1, e2) = coord e
printfn "-.a = (%f, %f)" e1 e2

/// Multiply a vector with a scalar using the overloaded operator from the module
let f = 3.0 *. a
let (f1, f2) = coord f
printfn "3 *. a = (%f, %f)" f1 f2

/// Make another vector, extract its coordinates and display them on screen.
let b = make(3.0, 4.0)
let (b1, b2) = coord b
printfn "b = (%f, %f)" b1 b2

/// Add 2 vectors with overloaded operator from the module
let c = a +. b
let (c1, c2) = coord c
printfn "a +. b = (%f, %f)" c1 c2

/// Subtract 2 vectors with overloaded operator from the module
let d = a -. b
let (d1, d2) = coord d
printfn "a -. b = (%f, %f)" d1 d2

/// Dot 2 vectors with overloaded operator from the module
let w = a &. b
printfn "a &. b = %f" w
