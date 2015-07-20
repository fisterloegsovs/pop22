namespace Firstform
open System
open System.Drawing
open System.Windows.Forms
 
module Main =
    //to add label in form
    let label =
        let temp = new Label()
        do temp.Size <- new System.Drawing.Size(100,100)
        do temp.Text<- "My First F# windows application!"
        do temp.AutoSize <- true
        do temp.Location <- new Drawing.Point(25,25)
        temp
    //to creat a form
    let form = new Form(BackColor = Color.Pink, Text = "First Window App")
    form.Controls.Add(label)
    Application.Run(form)
