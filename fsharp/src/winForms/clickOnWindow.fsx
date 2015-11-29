let form = new System.Windows.Forms.Form(Width= 400, Height = 300, Visible = true, Text = "Hello World") 
form.TopMost <- true
form.Click.Add (fun _ -> form.Text <- sprintf "form clicked at %i" System.DateTime.Now.Ticks)

[<System.STAThread>]
do System.Windows.Forms.Application.Run(form)
