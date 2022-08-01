module Collection
/// A collection of functions
///
/// How to compile:
/// <code>
/// fsharpc --doc:Collection.xml -a Collection.fsi Collection.fs
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/11/11

// The factorial function, undefined for negative values. Example: 6 = fact 3.
val fact : int->int

// A polynomial. Example: 16.0 = 1.0 * 3.0 ** 0.0 + 2.0 * 3.0 ** 1.0 + 1.0 * 3.0 ** 2.0 = polynomial [1.0; 2.0; 1.0] 3.0 
val polynomial : a:float list -> x:float -> float

// Convert a pair (2-tuple) of floats into a string. Example "(3.0, 4.0)" = pairToString (3.0, 4.0)
val pairToString : float * float -> string

// Convert any list into string. Example "(3.0, 4.0)" = listToString [3.0; 4.0]
val listToString : lst:obj list -> string
