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
let I = Image.fromFile "Barbara.jpg" |> Image.toGray
printfn "I : %g %g" (Image.min I) (Image.max I)
displayScaled "The gray version" I

let ( * ) I1 I2 =
  if not (Collections.Array2D.length1 I1 = Collections.Array2D.length1 I2) || not (Collections.Array2D.length2 I1 = Collections.Array2D.length2 I2) then
    failwith "The two images are not the same size"
  let mul (I1 : float [,]) (I2 : float [,]) i j =
    I1.[i,j] * I2.[i,j]
  Collections.Array2D.init<float> (Collections.Array2D.length1 I1) (Collections.Array2D.length2 I1) (mul I1 I2)

let J = Collections.Array2D.create (Collections.Array2D.length1 I) (Collections.Array2D.length2 I) 2.0
let K = J*I
//let a = 2.0*1.0

printfn "K : %g %g" (Image.min K) (Image.max K)
displayScaled "The multiplied version" K

printfn "Press crtl-c to quit"
System.Windows.Forms.Application.Run ()
