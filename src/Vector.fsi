namespace LinAlg
module Vector =
  type Vector
    val norm  : Vector -> float // Length of vector
    val make  : float * float -> Vector // Make vector
    val coord : Vector -> float * float // Get coordinates
    val ( +. ) : Vector -> Vector -> Vector // Vector addition
    val ( -. ) : Vector -> Vector -> Vector // Vector subtraction
    val ( ~-. ) : Vector -> Vector // Vector negation
    val ( *. ) : float -> Vector -> Vector // scalar-Vector multiplication
    val ( &. ) : Vector -> Vector -> float // Vector dot product

