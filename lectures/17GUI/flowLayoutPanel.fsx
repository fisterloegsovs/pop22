open System.Windows.Forms
open System.Drawing

// Create a window, a FlowLayoutPanel, 4 buttons, a checkbox, a panel, and 4 radiobuttons
let win = new Form(ClientSize=new Size(302, 356), Text="A Flowlayout Example")
let flowLayoutPanel = new FlowLayoutPanel(Location=new Point(47, 55), BorderStyle=BorderStyle.Fixed3D, WrapContents=true)
let buttonLst =
  [new Button(Text="Button0");
   new Button(Text="Button1");
   new Button(Text="Button2");
   new Button(Text="Button3")]
let panel = new Panel(Location=new Point (47, 190),BorderStyle=BorderStyle.Fixed3D)
let wrapContentsCheckBox = new CheckBox(Location=new Point (3, 3), Text="Wrap Contents")
let radioButtonLst =
  [(new RadioButton(Location=new Point(3, 34), Text="TopDown"), FlowDirection.TopDown);
   (new RadioButton(Location=new Point(3, 62), Text="BottomUp"), FlowDirection.BottomUp);
   (new RadioButton(Location=new Point(111, 34), Text="LeftToRight"), FlowDirection.LeftToRight);
   (new RadioButton(Location=new Point(111, 62), Text="RightToLeft"), FlowDirection.RightToLeft)]

// The window contains the panels which in turn contains the buttons, checkbox and radio buttons.
win.Controls.Add flowLayoutPanel
for btn in buttonLst do
  flowLayoutPanel.Controls.Add btn
win.Controls.Add panel
panel.Controls.Add (wrapContentsCheckBox)
for btn, dir in radioButtonLst do
  panel.Controls.Add (btn)

// Link wrapContentsCheckBox and flowLayoutPanel.WrapContents
wrapContentsCheckBox.Checked <- flowLayoutPanel.WrapContents
wrapContentsCheckBox.CheckedChanged.Add (fun _ -> flowLayoutPanel.WrapContents <- wrapContentsCheckBox.Checked)

// Link radio buttons and flowLayoutPanel.FlowDirection
for (btn, dir) in radioButtonLst do
  btn.Checked <- flowLayoutPanel.FlowDirection = dir
  btn.CheckedChanged.Add (fun _ -> flowLayoutPanel.FlowDirection <- dir)

// Create a window, add controls, and start event-loop
Application.Run win
