open Image

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
