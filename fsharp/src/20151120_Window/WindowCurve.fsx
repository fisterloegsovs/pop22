/// Open a window using winforms in Mono. The program opens a window
/// and draws a black triangle inside it. Wrap the display part in a
/// function and change coordinates to start in lower left corner.
///
/// How to compile:
/// <code>
/// fsharpc Window.fsx
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/11/19

open System.Drawing
open System.Windows.Forms

let display(title: string, c:Curve.Curve, color:Color, (pw:int, ph:int)) =
    let f (width, height) (x,y) = Point (int (round x), height - int (round y))
    let pArr = Array.ofList (List.map (f (pw, ph)) (Curve.toList c))
    let pen = new Pen (color)
    let draw (g:Graphics) = g.DrawLines (pen, pArr)
    let panel = new Panel (Dock = DockStyle.Fill)
    let winSize = Size (pw, ph)
    let win = new Form (Text = title, ClientSize = winSize)

    panel.Paint.Add (fun e -> draw (e.Graphics))
    win.Controls.Add panel
    Application.Run win

let listOfPoints = [(0.0,0.0); (20.0,170.0); (320.0,30.0); (0.0,0.0)]
let winSize = (450, 350)
let curve = Curve.fromList listOfPoints
display("My third window", curve, Color.Black, winSize)
