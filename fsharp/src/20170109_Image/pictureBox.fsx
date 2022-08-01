let display (aTitle: string, I : System.Drawing.Bitmap) =
  let winSize = System.Drawing.Size (I.Width, I.Height)
  let win = new System.Windows.Forms.Form ()
  win.Text <- aTitle
  win.ClientSize <- winSize
  win.MaximizeBox <- false
  win.MinimizeBox <- false
  let pb = new System.Windows.Forms.PictureBox ()
  pb.Image <- I
  pb.Size <- I.Size
  win.Controls.Add(pb)
  win.Show();
  win
  
let C = new System.Drawing.Bitmap ("Barbara.jpg")
display ("The original image", C) |> ignore

printfn "Press crtl-c to quit"
System.Windows.Forms.Application.Run ()
