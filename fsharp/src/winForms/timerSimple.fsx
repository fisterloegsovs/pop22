let showtime (lbl : System.Windows.Forms.Label) t =
  let dtobject = System.Convert.ToString System.DateTime.Now
  let timepart = dtobject.Substring 10
  lbl.Text <- timepart

let quit (timeObj : System.Windows.Forms.Timer) (form : System.Windows.Forms.Form) _ =
  timeObj.Stop ()
  form.Close ()

let gettimeform = new System.Windows.Forms.Form(Text = "Show Current Time")
let lblmsg = new System.Windows.Forms.Label(Top = 60, Left = 60, Width = 130, Text = "Current Time of System")
let lbltime = new System.Windows.Forms.Label (Top = 100, Left = 60, Width = 115)  
let exitbutton = new System.Windows.Forms.Button (Top = 140, Left = 80, Text = "Exit")
let timeobject = new System.Windows.Forms.Timer (Interval = 1000, Enabled = true)  

gettimeform.Controls.Add lblmsg
timeobject.Tick.Add (showtime lbltime)
gettimeform.Controls.Add lbltime
gettimeform.Controls.Add exitbutton
exitbutton.Click.Add (quit timeobject gettimeform)

gettimeform.Show ()  
System.Windows.Forms.Application.Run (gettimeform)  
