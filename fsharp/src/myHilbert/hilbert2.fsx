open System.Drawing
open System.Windows.Forms
open Curve

let point2DrawingPoint (p : float list) =
  Point (int (round p.[0]), int (round p.[1]))

let curve2DrawingPointArray c =
  Array.ofList (List.map point2DrawingPoint c)

let rec hilbertA n (dir, c) =
  if n > 0 then
    ((dir, c) |> turnLeft |> hilbertB (n-1) |> draw |> turnRight |> hilbertA (n-1) |> draw |> hilbertA (n-1) |> turnRight |> draw |> hilbertB (n-1) |> turnLeft)
  else
    (dir, c)
and hilbertB n (dir, c) = 
  if n > 0 then
    ((dir, c) |> turnRight |> hilbertA (n-1) |> draw |> turnLeft |> hilbertB (n-1) |> draw |> hilbertB (n-1) |> turnLeft |> draw |> hilbertA (n-1) |> turnRight)
  else
    (dir, c)

let (dir, C) = hilbertA 6 (0.0, [[0.0; 0.0]])
printfn "C.[0]: %A, min: %A, max: %A" C.[0] (minimum C) (maximum C)

let title = "Hilbert's curve"
let width = 320;
let height = width;
let cMin = minimum C
let cMax = maximum C
let D = scaleCurve (0.99 * (float width) / (cMax.[0] - cMin.[0])) (translateCurve (scalePoint -1.0 cMin) C)
printfn "D.[0]: %A, min: %A, max: %A" D.[0] (minimum D) (maximum D)

let pArr = curve2DrawingPointArray D
let penColor = Color.Black
let pen = new Pen (penColor)
let panel = new Panel (Dock = DockStyle.Fill)
panel.Paint.Add (fun e -> e.Graphics.DrawLines (pen, pArr))

let winSize = Size (width, height)
let win = new Form (Text = title, ClientSize = winSize)
win.Controls.Add panel
Application.Run win
