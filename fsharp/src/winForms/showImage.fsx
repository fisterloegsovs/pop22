open System
open System.Windows.Forms
open System.Drawing

let form = new Form()
let pb = new PictureBox()
pb.Image <- Image.FromFile("/Users/sporring/repositories/SIP/2013/exercises/Barbara.jpg")
pb.SizeMode <- PictureBoxSizeMode.AutoSize
form.Controls.Add(pb)

[<STAThread>]
do
    Application.Run(form)
