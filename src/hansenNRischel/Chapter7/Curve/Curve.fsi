// Code from Hansen and Rischel: Functional Programming using F#     16/12 2012
// Chapter 7: Modules

// From Section 7.9 Example: piecewise linear plane curves 
// Signature file for a Curve

module Curve
[<Sealed>]
type Curve =
    static member ( + ) : Curve * Curve -> Curve
    static member ( * ) : float * Curve -> Curve
    static member ( |^) : Curve * float -> Curve
    static member ( |^) : Curve * int   -> Curve
    static member (-->) : Curve * (float * float) -> Curve
    static member (><)  : Curve * float -> Curve
val point       : float * float -> Curve
val verticRefl  : Curve -> float -> Curve
val boundingBox : Curve -> (float * float) * (float * float)
val width       : Curve -> float
val height      : Curve -> float
val toList      : Curve -> (float * float) list

