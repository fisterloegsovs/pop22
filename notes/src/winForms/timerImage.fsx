let showImage (pb : System.Windows.Forms.PictureBox) (im1 : System.Drawing.Image) (im2 : System.Drawing.Image) t =
  if pb.Image = im1 then
    pb.Image <- im2
  else
    pb.Image <- im1

let setupForms (im1 : System.Drawing.Image) (im2 : System.Drawing.Image) =
  let form = new System.Windows.Forms.Form(Text = "Switch image every second")
  let pictureBox = new System.Windows.Forms.PictureBox(SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize)
  let timer = new System.Windows.Forms.Timer (Interval = 1000, Enabled = true)  

  pictureBox.Image <- im1
  form.Controls.Add (pictureBox)
  timer.Tick.Add (showImage pictureBox im1 im2)
  form
  
let barbara = System.Drawing.Image.FromFile("Barbara.jpg")
let lena = System.Drawing.Image.FromFile("lena.jpg")
  
let form = setupForms lena barbara
form.Show ()  
System.Windows.Forms.Application.Run (form)  
