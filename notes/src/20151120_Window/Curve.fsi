/// Code from Hansen and Rischel: Functional Programming using F# 16/12 2012
/// Chapter 7: Modules
/// From Section 7.9 Example: piecewise linear plane curves 
/// Signature file for a Curve
///
/// Comments added by Jon Sporring, 2015/11/19

module Curve
[<Sealed>]
/// A curve is a sequence of 1 or more 2 dimensional points
type Curve =
    static member ( + ) : Curve * Curve -> Curve // connect end of one curve to the beginning of the second
    static member ( * ) : float * Curve -> Curve // scale points around the first point
    static member ( |^) : Curve * float -> Curve // rotate points around the first point
    static member ( |^) : Curve * int   -> Curve // rotate points around the first point
    static member (-->) : Curve * (float * float) -> Curve // translate curve such that the first point is as specified
    static member (><)  : Curve * float -> Curve // flip curve around x=a
val point       : float * float -> Curve // make a curve consisting of a single point
val verticRefl  : Curve -> float -> Curve // flip curve around y=a
val boundingBox : Curve -> (float * float) * (float * float) // the coordinates of the bounding box of a curve
val width       : Curve -> float // the width of a curve
val height      : Curve -> float // the height of a curve
val toList      : Curve -> (float * float) list // convert curve to list of pairs of floats
val fromList    : (float * float) list -> Curve // convert a list of pairs of floats to a Curve
