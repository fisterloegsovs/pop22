let display (aTitle: string) (I : System.Drawing.Color [,]) =
  let winSize = System.Drawing.Size (Collections.Array2D.length1 I, Collections.Array2D.length2 I)
  let win = new System.Windows.Forms.Form ()
  win.Text <- aTitle
  win.ClientSize <- winSize
  win.MaximizeBox <- false
  win.MinimizeBox <- false
  let pb = new System.Windows.Forms.PictureBox ()
  pb.Image <- Image.toBitmap I
  pb.Size <- winSize
  win.Controls.Add(pb)
  win.Show();
  win

let displayScaled (aTitle: string) (I : float [,]) =
  let N = Image.normalize I 0.0 255.0
  let C = Image.toColor N
  display aTitle C

// Read an image from file and show some of its values
let I = Image.fromFile "Barbara.jpg"
printfn "I : %A" I.[0..3,0..3]

//(*
// Render the image
display "The original image" I

// Extract each color plane, analyze, and render them
let R = Array2D.map (fun (c : System.Drawing.Color) -> float c.R) I
printfn "R : %g %g" (Image.min R) (Image.max R)
display "The original image's red channel" (R |> Image.toColor)
let G = Array2D.map (fun (c : System.Drawing.Color) -> float c.G) I
printfn "G : %g %g" (Image.min G) (Image.max G)
display "The original image's green channel" (G |> Image.toColor)
let B = Array2D.map (fun (c : System.Drawing.Color) -> float c.B) I
printfn "B : %g %g" (Image.min B) (Image.max B)
display "The original image's blue channel" (B |> Image.toColor)

// Convert the image to gray value, analyze, and display it
let IGray = Image.toGray I
printfn "IGray : %g %g" (Image.min IGray) (Image.max IGray)
displayScaled "The gray version" IGray

// Calculate the histogram of the gray image
let (bins, hist) = Image.histogram IGray 30
printfn "hist : %A" (Array.zip bins hist)

// Threshold the image, analyze, and display it
let T = Array2D.map (fun (v : float) -> float (System.Convert.ToInt32 (v > 57.0))) IGray
printfn "T : %g %g" (Image.min T) (Image.max T)
displayScaled "The thresholded version" T

// Calculate the gradient magnitude, analyze, and display it
let IGrad = IGray |> Image.d |> Image.grad
printfn "IGrad : %g %g" (Image.min IGrad) (Image.max IGrad)
displayScaled "The gradient of the gray image" IGrad

// Smooth the image, calculate the gradient magnitude, analyze, and display it
for sigma in [ 1.0; 5.0 ] do
  printfn "sigma = %f" sigma
  let width = int (4.0 * sigma + 1.0)
  let Gauss = Image.gauss width width sigma
  printfn "Gauss : %g %g %g" (Image.min Gauss) (Image.max Gauss) (Image.sum Gauss)
  let J = Image.convolve IGray Gauss
  printfn "J : %g %g" (Image.min J) (Image.max J)
  displayScaled (sprintf "The smoothed gray image (sigma = %f)" sigma) J |> ignore

  let JGrad = J |> Image.d |> Image.grad
  printfn "JGrad : %g %g" (Image.min JGrad) (Image.max JGrad)
  displayScaled (sprintf "The normalized gradient of the smoothed image (sigma = %.1f)" sigma) JGrad |> ignore
//*)

printfn "Press crtl-c to quit"
System.Windows.Forms.Application.Run ()
