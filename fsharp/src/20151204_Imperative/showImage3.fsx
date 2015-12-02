let bitmap2GrayArray2D (I : System.Drawing.Bitmap) =
  let pixel2Gray (I : System.Drawing.Bitmap) (i : int) (j : int) =
    let color = I.GetPixel (i,j)
    ((float color.R) + (float color.G) + (float color.B)) / 3.0 
  Collections.Array2D.init<float> I.Width I.Height (pixel2Gray I)

let grayArray2D2Bitmap (I : float [,]) =
  let width = Collections.Array2D.length1 I
  let height = Collections.Array2D.length2 I
  let J = new System.Drawing.Bitmap (width, height)
  for i = 0 to width - 1 do
    for j = 0 to height - 1 do
      let gray = int (I.[i,j])
      let newColor = System.Drawing.Color.FromArgb (gray, gray, gray)
      J.SetPixel (i,j, newColor)
  J

let array2dMax (I : float [,]) =
  let mutable currentMax = System.Double.MinValue
  for i = 1 to (Collections.Array2D.length1 I) - 1 do
    for j = 1 to (Collections.Array2D.length2 I) - 1 do
      currentMax <- max currentMax I.[i,j]
  currentMax

let array2dMin (I : float [,]) =
  let mutable currentMin = System.Double.MaxValue
  for i = 1 to (Collections.Array2D.length1 I) - 1 do
    for j = 1 to (Collections.Array2D.length2 I) - 1 do
      currentMin <- min currentMin I.[i,j]
  currentMin

let array2dSum (I : float [,]) =
  let mutable s = 0.0
  for i = 1 to (Collections.Array2D.length1 I) - 1 do
    for j = 1 to (Collections.Array2D.length2 I) - 1 do
      s <- s + I.[i,j]
  s

let normalize (I : float [,]) (newMin : float) (newMax : float) =
  let minI = array2dMin I
  let maxI = array2dMax I
  for i = 1 to (Collections.Array2D.length1 I) - 1 do
    for j = 1 to (Collections.Array2D.length2 I) - 1 do
      I.[i,j] <- ( I.[i,j] - minI ) * (newMax - newMin) / ( maxI - minI) + newMin
  I

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
    for m = 1 to (Collections.Array2D.length1 K) - 1 do
      for n = 1 to (Collections.Array2D.length2 K) - 1 do
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

let display (aTitle: string, I : System.Drawing.Bitmap, J : System.Drawing.Bitmap) =
  let width  = I.Width + J.Height
  let height  = max I.Height J.Height
  let winSize = System.Drawing.Size (width, height)
  let win = new System.Windows.Forms.Form (Text = aTitle, ClientSize = winSize, MaximizeBox = false, MinimizeBox = false)

  let pbLoc = new System.Drawing.Point (0, 0)
  let pb = new System.Windows.Forms.PictureBox (Image = I, Size = I.Size, Location = pbLoc)
  win.Controls.Add(pb)

  let pb2Loc = new System.Drawing.Point (I.Width, 0)
  let pb2 = new System.Windows.Forms.PictureBox (Image = J, Size = J.Size, Location = pb2Loc )
  win.Controls.Add(pb2)

  System.Windows.Forms.Application.Run win

let C = new System.Drawing.Bitmap ("Barbara.jpg")
let I = bitmap2GrayArray2D C
printfn "I : %g %g" (array2dMin I) (array2dMax I)
let sigma = 3.0
let G = gauss (int (4.0 * sigma + 1.0), int (4.0 * sigma + 1.0), sigma)
printfn "G : %g %g %g" (array2dMin G) (array2dMax G) (array2dSum G)
let J = convolve (I, G)
printfn "J : %g %g" (array2dMin J) (array2dMax J)
let dJ = grad (d J)
printfn "dJ : %g %g" (array2dMin dJ) (array2dMax dJ)
display ("An image", C, grayArray2D2Bitmap I)
//display ("An image", grayArray2D2Bitmap I, grayArray2D2Bitmap J)
//display ("An image", grayArray2D2Bitmap (normalize I 0.0 255.0), grayArray2D2Bitmap (normalize J 0.0 255.0))
//display ("An image", grayArray2D2Bitmap (normalize J 0.0 255.0), grayArray2D2Bitmap (normalize dJ 0.0 255.0))
