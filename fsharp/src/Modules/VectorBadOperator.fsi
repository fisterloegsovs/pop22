module Vector
type Vector
val ( + ) : Vector -> Vector -> Vector // Vector addition
val make  : float * float -> Vector // Make vector
val coord : Vector -> float * float // Get coordinates
