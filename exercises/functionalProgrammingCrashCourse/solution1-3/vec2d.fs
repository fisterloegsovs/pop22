/// A 2 dimensional vector library.
/// Vectors are represented as pairs of floats module vec2d
module vec2d
/// The length of a vector
let len (x:float,y:float):float = sqrt(x * x + y * y)
/// The angle of a vector
let ang (x:float,y:float):float = atan2 y x
/// Multiplication of a float and a vector
let scale (a:float) (x:float,y:float):float*float = (a * x, a * y)
/// Addition of two vectors
let add (x1:float,y1:float) (x2:float,y2:float):float*float = 
  (x1 + x2, y1 + y2)
/// Dot product of two vectors
let dot (x1:float,y1:float) (x2:float,y2:float):float = 
  x1 * x2 + y1 * y2
