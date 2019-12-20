open System.Windows.Forms    
open System.Drawing   

let win = new Form () // make a window form
win.ClientSize <- Size (200, 100)

// make a label to show time
let label = new Label()  
win.Controls.Add label
label.Width <- 200
label.Location <- new Point (10, 20)
label.Text <- "Hello World"
label.BackColor <- Color.White
label.Height <- 20

Application.Run win // start event-loop
