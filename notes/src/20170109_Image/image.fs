module Image

/// Convert a Bitmap to an Array2D
let fromBitmap (bmp : System.Drawing.Bitmap) =
  let getValue (B : System.Drawing.Bitmap) i j = B.GetPixel (i,j)
  Collections.Array2D.init<System.Drawing.Color> bmp.Width bmp.Height (getValue bmp)

/// Convert an Array2D to a Bitmap
let toBitmap (I : System.Drawing.Color [,]) =
  let J = new System.Drawing.Bitmap (Collections.Array2D.length1 I, Collections.Array2D.length2 I)
  for i = 0 to Collections.Array2D.length1 I - 1 do
    for j = 0 to Collections.Array2D.length2 I - 1 do
      J.SetPixel (i,j, I.[i,j])
  J

/// Read a Bitmap from file and convert it to an Array2D
let fromFile (name : string) =
  new System.Drawing.Bitmap(name) |> fromBitmap

/// Write a Bitmap to file
let toFile (name : string) (I : System.Drawing.Color [,]) =
  let bmp = toBitmap I
  bmp.Save(name, System.Drawing.Imaging.ImageFormat.Png)
  bmp

/// Convert a Color to a float value
let colorToGray (c : System.Drawing.Color) =
  (float c.R + float c.G + float c.B)/3.0

/// Convert an Array2D of Color to an Array2D of float values
let toGray (I : System.Drawing.Color [,]) =
  Collections.Array2D.map colorToGray I

/// Convert a float value to a Color
let grayToColor (g : float) =
  System.Drawing.Color.FromArgb (int g, int g, int g)

/// Convert an Array2D of float values to an Array2D of Colors
let toColor (I : float [,]) =
  Collections.Array2D.map grayToColor I

/// Fold the values of an Array2D of floats into a single float
let fold (f : float -> float -> float) (initial: float) (I : float [,])=
  let mutable v = initial
  for i = 0 to (Collections.Array2D.length1 I) - 1 do
    for j = 0 to (Collections.Array2D.length2 I) - 1 do
      v <- f v I.[i,j]
  v

/// Combine 2 Array2D of floats into a single Array2D of tuples
let zip (I1 : float [,]) (I2 : float [,]) =
  if not (Collections.Array2D.length1 I1 = Collections.Array2D.length1 I2) || not (Collections.Array2D.length2 I1 = Collections.Array2D.length2 I2) then
    failwith "The two images are not the same size"
  let combine (I1 : float [,]) (I2 : float [,]) i j =
    (I1.[i,j], I2.[i,j])
  Collections.Array2D.init<float * float> (Collections.Array2D.length1 I1) (Collections.Array2D.length2 I1) (combine I1 I2)

/// Calculate the maximum value of a an Array2D of floats
let max (I : float [,]) =
  fold max System.Double.MinValue I

/// Calculate the minimum value of a an Array2D of floats
let min (I : float [,]) =
  fold min System.Double.MaxValue I

/// Calculate the sum of all values of an Array2D of floats
let sum (I : float [,]) =
  fold (+) 0.0 I

/// Linearly stretch the values of Array2D of floats to a new set of minimum and maximum values
let normalize (I : float [,]) (newMin : float) (newMax : float) =
  let minI = min I
  let maxI = max I
  let f oldMin oldMax newMin newMax v = ( v - oldMin ) * (newMax - newMin) / ( oldMax - oldMin) + newMin
  Collections.Array2D.map (f minI maxI newMin newMax ) I

/// Calculate the histogram of values of an Array2D of floats in n+1 bins
let histogram (I : float [,]) (n : int) =
  let a = min I
  let b = max I
  let h = Array.create (n+1) 0
  for i = 0 to (Collections.Array2D.length1 I) - 1 do
    for j = 0 to (Collections.Array2D.length2 I) - 1 do
      let idx = int ((I.[i,j]-a) * (float n) / (b-a))
      h.[idx] <- h.[idx] + 1
  let v = [|a .. (b-a)/(float n) .. b|]
  (v, h)

/// Calculate the first order derivative of an Array2D of floats.
let d (I : float [,]) =
  let width = Collections.Array2D.length1 I
  let height = Collections.Array2D.length2 I
  let dx (I : float [,]) (i : int) (j : int) =
    I.[(i+1) % width, j] - I.[i, j]
  let dy (I : float [,]) (i : int) (j : int) =
    I.[i, (j+1) % height] - I.[i, j]
  let Ix = Collections.Array2D.init<float> width height (dx I)
  let Iy = Collections.Array2D.init<float> width height (dy I)
  zip Ix Iy

/// Calculate the Euclidean lenght of a tuple
let euclideanLength pair =
  sqrt ((fst pair) ** 2.0 + (snd pair) ** 2.0)
    
/// Calculate the gradient magnitude of an Array2D of tuples
let grad (dI : (float * float) [,]) =
  Collections.Array2D.map euclideanLength dI
  
/// Convolve two images with each other
let convolve (I : float [,]) (K : float [,]) =
  let conv (I : float [,]) (K : float [,]) (i : int) (j : int) =
    let mutable sum = 0.0
    for m = 0 to (Collections.Array2D.length1 K) - 1 do
      for n = 0 to (Collections.Array2D.length2 K) - 1 do
        if (i - m >= 0) && (i - m < Collections.Array2D.length1 I) && (j - n >= 0) && (j - n < Collections.Array2D.length2 I) then
          sum <- sum + I.[i - m, j - n] * K.[m, n]
    sum
  let width = (Collections.Array2D.length1 I) + (Collections.Array2D.length1 K)
  let height = (Collections.Array2D.length2 I) + (Collections.Array2D.length2 K)
  Collections.Array2D.init<float> width height (conv I K)

/// Create an image of a Gaussian function centered in the middle of the image
let gauss (width : int) (height : int) (sigma : float) =
  let g (width : int) (height : int) (sigma : float) (i : int) (j : int) = 
    let a = (float i) - (float (width - 1))/2.0
    let b = (float j) - (float (height - 1))/2.0
    (exp(-(a**2.0 + b**2.0) / (2.0 * sigma ** 2.0))) / (2.0 * System.Math.PI * sigma**2.0)
  Collections.Array2D.init<float> width height (g width height sigma)
