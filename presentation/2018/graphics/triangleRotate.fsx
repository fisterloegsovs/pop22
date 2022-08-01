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

let rotate offset angle points =
  let negativeOffset = scalePoint -1.0 offset
  points |> translatePoints negativeOffset |> rotatePoints angle |> translatePoints offset

let width = 400
let height = 400
let triangle = [200.0, 200.0; 300.0, 200.0; 300.0, 300.0; 200.0, 200.0]
let n = 10;
let pi = 3.141592
let coords = List.collect (fun a -> rotate (200.0, 200.0) a triangle) [0.0 .. 2.0*pi/(float n) .. 2.0*pi]

let win = new Form ()
win.Text <- "Rotated triangles"
win.BackColor <- Color.White
win.ClientSize <- Size (width, height)
win.Paint.Add (drawPoints coords)

// Start the event-loop.
Application.Run win
