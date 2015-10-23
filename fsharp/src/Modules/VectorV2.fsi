module Vector
[<Sealed>]
type Vector =
  static member ( ~- ) : Vector -> Vector // Vector negation
  static member ( + ) : Vector * Vector -> Vector // Vector addition
  static member ( - ) : Vector * Vector -> Vector // Vector subtraction
  static member ( * ) : float * Vector -> Vector // scalar-Vector multiplication
  static member ( * ) : Vector * Vector -> float // Vector dot product
val make : float * float -> Vector
val coord: Vector -> float * float
val norm : Vector -> float
