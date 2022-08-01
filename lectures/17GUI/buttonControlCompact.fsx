open System.Windows.Forms
open System.Drawing

// Model: a state 'clicked' that counts how many times an event has occurred
let mutable clicked = 0
let message () = sprintf "Clicked %d times" clicked 
let update () = clicked <- clicked + 1
  
// View: A window containing a label and a button
let win = new Form(ClientSize=Size(140, 120))
let label = new Label(Location=new Point(20, 20), Width=120)
let button = new Button(Size=new Size(100, 40), Location=new Point(20, 60), Text="Click me")
win.Controls.Add label
win.Controls.Add button

// Connect model and view and start the event-loop
label.Text <- message ()
button.Click.Add (fun e -> update (); label.Text <- message ())
Application.Run win
