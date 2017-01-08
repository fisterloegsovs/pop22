module Image

let bitmap2GrayArray2D (I : System.Drawing.Bitmap) =
  let pixel2Gray (I : System.Drawing.Bitmap) (i : int) (j : int) =
    let color = I.GetPixel (i,j)
    ((float color.R) + (float color.G) + (float color.B)) / 3.0 
  Collections.Array2D.init<float> I.Width I.Height (pixel2Gray I)

let grayArray2D2Bitmap (I : float [,]) =
  let J = new System.Drawing.Bitmap (Collections.Array2D.length1 I, Collections.Array2D.length2 I)
  let f (J : System.Drawing.Bitmap) (i : int) (j : int) (v : float) =
    let gray = int v
    let newColor = System.Drawing.Color.FromArgb (gray, gray, gray)
    J.SetPixel (i,j, newColor)
  Collections.Array2D.iteri (f J) I
  J
  
let fold (I : float [,], f : float->float -> float, initial: float) =
  let mutable v = initial
  for i = 0 to (Collections.Array2D.length1 I) - 1 do
    for j = 0 to (Collections.Array2D.length2 I) - 1 do
      v <- f v I.[i,j]
  v

let array2dMax (I : float [,]) =
  fold (I, max, System.Double.MinValue)

let array2dMin (I : float [,]) =
  fold (I, min, System.Double.MaxValue)

let array2dSum (I : float [,]) =
  fold (I, (+), 0.0)

let normalize (I : float [,], newMin : float, newMax : float) =
  let minI = array2dMin I
  let maxI = array2dMax I
  let f oldMin oldMax newMin newMax v = ( v - oldMin ) * (newMax - newMin) / ( oldMax - oldMin) + newMin
  Collections.Array2D.map (f minI maxI newMin newMax ) I

let d (I : float [,]) =
  let width = Collections.Array2D.length1 I
  let height = Collections.Array2D.length2 I
  let dx (I : float [,]) (i : int) (j : int) =
    I.[(i+1) % width, j] - I.[i, j]
  let dy (I : float [,]) (i : int) (j : int) =
    I.[i, (j+1) % height] - I.[i, j]
  let Ix = Collections.Array2D.init<float> width height (dx I)
  let Iy = Collections.Array2D.init<float> width height (dy I)
  (Ix, Iy)
  
let grad (Ix : float [,], Iy : float [,]) =
  let width = Collections.Array2D.length1 Ix
  let height = Collections.Array2D.length2 Ix
  if not (width = Collections.Array2D.length1 Iy) || not (height = Collections.Array2D.length2 Iy) then
    failwith "The two images are not the same size"
  let g (Ix : float [,]) (Iy : float [,]) (i : int) (j : int) =
    sqrt (Ix.[i, j] ** 2.0 + Iy.[i, j] ** 2.0)
  Collections.Array2D.init<float> width height (g Ix Iy)

let convolve (I : float [,], K : float [,]) =
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
  
let gauss (width : int, height : int, sigma : float) =
  let g (width : int) (height : int) (sigma : float) (i : int) (j : int) = 
    let a = (float i) - (float (width - 1))/2.0
    let b = (float j) - (float (height - 1))/2.0
    (exp(-(a**2.0 + b**2.0) / (2.0 * sigma ** 2.0))) / (2.0 * System.Math.PI * sigma**2.0)
  Collections.Array2D.init<float> width height (g width height sigma)

