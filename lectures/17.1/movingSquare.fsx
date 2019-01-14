open System.Windows.Forms    
open System.Drawing   

let win = new Form () // make a window form
win.ClientSize <- Size (200, 50)

let mutable delta = Point (0,0)
let mutable dir = Point (1,1)
let polygonSz = Point (10,10);
let polygon = [|Point (0,0); Point (polygonSz.X,0); polygonSz; Point (0,polygonSz.Y); Point (0,0)|]
let paint (e : PaintEventArgs) : unit =
  let pen = new Pen (Color.Black)
  if delta.X + dir.X < 0 || delta.X + dir.X + polygonSz.X > win.ClientSize.Width - 1 then
    dir <- Point (-dir.X, dir.Y);
  if delta.Y + dir.Y < 0 || delta.Y + dir.Y + polygonSz.Y > win.ClientSize.Height - 1 then
    dir <- Point (dir.X, -dir.Y);
  delta <- Point (delta.X + dir.X, delta.Y + dir.Y)
  let add (p : Point) = Point (p.X + delta.X, p.Y + delta.Y)
  let points = Array.map add polygon
  e.Graphics.DrawLines (pen, points)
win.Paint.Add paint

// make a timer
let timer = new Timer()
timer.Interval <- 10 // create an event every 1000 millisecond
timer.Enabled <- true // activiate the timer
timer.Tick.Add (fun e -> win.Refresh())

Application.Run win // start event-loop
