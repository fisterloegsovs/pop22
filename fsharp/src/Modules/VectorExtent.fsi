module Vector
[<Sealed>]
type Vector =
  static member ( ~+. ) : Vector -> float // Vector dot product
val make : float * float -> Vector
val norm : Vector -> float
