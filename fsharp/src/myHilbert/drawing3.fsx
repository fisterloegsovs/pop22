open System.Drawing
open System.Windows.Forms
open Curve

let point2DrawingPoint (p : float list) =
  Point (int (round p.[0]), int (round p.[1]))

let curve2DrawingPointArray c =
  Array.ofList (List.map point2DrawingPoint c)

let c = [
  [0.0; 0.0];
  [0.0; 1.0];
  [1.0; 1.0];
  [1.0; 0.0]]
let c2 = translateCurveTo c.[3] (rotateCurve (-3.1415/2.0) c)
let c3 = translateCurveTo c2.[3] (rotateCurve (-3.1415) c)
let c4 = translateCurveTo c3.[3] (rotateCurve (-3.1415*3.0/2.0) c)
let C = c @ c2 @ c3 @ c4

let CScaled = scaleCurve 50.0 C
let centroid = center CScaled
let CScaleTranslated = translateCurve (scalePoint -1.0 centroid) CScaled

let title = "Peano's curve"
let width = 320;
let height = width;
let pArr = curve2DrawingPointArray (translateCurve [float width/2.0; float height/2.0] CScaleTranslated)

let penColor = Color.Black
let pen = new Pen (penColor)
let panel = new Panel (Dock = DockStyle.Fill)
panel.Paint.Add (fun e -> e.Graphics.DrawLines (pen, pArr))

let winSize = Size (width, height)
let win = new Form (Text = title, ClientSize = winSize)
win.Controls.Add panel
Application.Run win
