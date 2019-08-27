open System.Windows.Forms
open System.Drawing
open Graph

/// Draw a polygon with a specific color
let drawPoints (coords : polyline) (e : PaintEventArgs) =
  let pairToPoint p =
    Point (int (round (fst p)), int (round (snd p)))
  let Pen = new Pen (Color.Black, single 1.0)
  let Points = Array.map pairToPoint (List.toArray coords)
  e.Graphics.DrawLines (Pen, Points)

let width = 400
let height = 400
let coords = [200.0, 200.0; 300.0, 200.0; 300.0, 300.0; 200.0, 200.0]

let win = new Form ()
win.Text <- "A triangle"
win.BackColor <- Color.White
win.ClientSize <- Size (width, height)
win.Paint.Add (drawPoints coords)

// Start the event-loop.
Application.Run win
