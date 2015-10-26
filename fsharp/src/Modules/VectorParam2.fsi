module Vector
[<Sealed>]
type Vector =
  static member ( +. ) : Vector * Vector -> Vector
val make : float * float -> Vector
val coord : Vector -> float * float
