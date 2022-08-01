module Vector
type Vector
val ( ~-. ) : Vector -> Vector // Vector negation
val ( +. ) : Vector -> Vector -> Vector // Vector addition
val ( -. ) : Vector -> Vector -> Vector // Vector subtraction
val ( *. ) : float -> Vector -> Vector // scalar-Vector multiplication
val ( &. ) : Vector -> Vector -> float // Vector dot product
val norm  : Vector -> float // Length of vector
val make  : float * float -> Vector // Make vector
val coord : Vector -> float * float // Get coordinates
