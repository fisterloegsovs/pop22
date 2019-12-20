open System.Drawing
open System.Windows.Forms

// Create a window with a panel, label and a textbox
let win = new Form(ClientSize=new Size (200, 100))
let panel = new Panel(ClientSize=new Size(160, 60), Location=new Point(20,20), BorderStyle=BorderStyle.Fixed3D)
let label = new Label(ClientSize=new Size(120, 20), Location=new Point(15,5), Text="Input")
let textBox = new TextBox(ClientSize=new Size(120, 20), Location=new Point(20,25), Text="Initial text")

win.Controls.Add panel // Add panel to window
panel.Controls.Add label // add label to panel
panel.Controls.Add textBox // add textbox to panel

Application.Run win // Start the event-loop
