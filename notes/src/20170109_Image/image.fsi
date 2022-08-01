module Image
/// A set of functions for image processing

/// Convert a Bitmap to an Array2D
val fromBitmap : bmp:System.Drawing.Bitmap -> System.Drawing.Color [,]

/// Convert an Array2D to a Bitmap
val toBitmap : I:System.Drawing.Color [,] -> System.Drawing.Bitmap

/// Read a Bitmap from file and convert it to an Array2D
val fromFile : name:string -> System.Drawing.Color [,]

/// Write a Bitmap to file
val toFile : name:string -> I:System.Drawing.Color [,] -> System.Drawing.Bitmap

/// Convert a Color to a float value
val colorToGray : c:System.Drawing.Color -> float

/// Convert an Array2D of Color to an Array2D of float values
val toGray : I:System.Drawing.Color [,] -> float [,]

/// Convert a float value to a Color
val grayToColor : g:float -> System.Drawing.Color

/// Convert an Array2D of float values to an Array2D of Colors
val toColor : I:float [,] -> System.Drawing.Color [,]

/// Fold the values of an Array2D of floats into a single float
val fold : f:(float -> float -> float) -> initial:float -> I:float [,] -> float

/// Combine 2 Array2D of floats into a single Array2D of tuples
val zip : I1:float [,] -> I2:float [,] -> (float * float) [,]

/// Calculate the maximum value of a an Array2D of floats
val max : I:float [,] -> float

/// Calculate the minimum value of a an Array2D of floats
val min : I:float [,] -> float

/// Calculate the sum of all values of an Array2D of floats
val sum : I:float [,] -> float

/// Linearly stretch the values of Array2D of floats to a new set of minimum and maximum values
val normalize : I:float [,] -> newMin:float -> newMax:float -> float [,]

/// Calculate the histogram of values of an Array2D of floats in n+1 bins
val histogram : I:float [,] -> n:int -> float [] * int []

/// Calculate the first order derivative of an Array2D of floats.
val d : I:float [,] -> (float * float) [,]

/// Calculate the Euclidean lenght of a tuple
val euclideanLength : float * float -> float

/// Calculate the gradient magnitude of an Array2D of tuples
val grad : dI:(float * float) [,] -> float [,]

/// Convolve two images with each other
val convolve : I:float [,] -> K:float [,] -> float [,]

/// Create an image of a Gaussian function centered in the middle of the image
val gauss : width:int -> height:int -> sigma:float -> float [,]
