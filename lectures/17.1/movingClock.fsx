open System.Windows.Forms    
open System.Drawing   

let win = new Form () // make a window form
win.ClientSize <- Size (200, 50)

// make a label to show time
let label = new Label()  
win.Controls.Add label
label.Text <- string System.DateTime.Now // get present time and date
let textSz = TextRenderer.MeasureText(label.Text,label.Font)
label.Width <- textSz.Width
label.Height <- textSz.Height
label.BackColor <- Color.White

// make a timer and link to label
let timer = new Timer()
timer.Interval <- 100 // create an event every 1000 millisecond
timer.Enabled <- true // activiate the timer
let mutable pos = (0,0)
let mutable dir = (1,1)
let performTick (e : System.EventArgs) =
  printfn "%A %A" pos dir
  if fst pos + fst dir > win.ClientSize.Width - label.Width - 1 || fst pos + fst dir < 0 then
    dir <- (-fst dir, snd dir);
  if snd pos + snd dir > win.ClientSize.Height - label.Height - 1|| snd pos + snd dir < 0 then
    dir <- (fst dir, -snd dir);
  pos <- (fst pos + fst dir, snd pos + snd dir)
  label.Location <- Point (fst pos, snd pos);
  label.Text <- string System.DateTime.Now
timer.Tick.Add performTick

Application.Run win // start event-loop
