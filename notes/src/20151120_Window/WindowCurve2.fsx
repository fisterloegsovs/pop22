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

let mul a (x, y) = (a*x, a*y);
let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2);
let intToFloat (x:int, y:int) = (float x, float y)
let center lst = 
  let sumOfPoints = List.fold (fun (x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) (0.0, 0.0) lst
  mul (1.0 / (float (lst.Length))) sumOfPoints

let dv = add (mul 0.5 (intToFloat winSize)) (mul -1.0 (center listOfPoints))
let curve = Curve.fromList listOfPoints
let curve2 = curve --> dv

printfn "Original center of mass: %A" (center listOfPoints)
printfn "Desired center of mass: %A" (mul 0.5 (intToFloat winSize))
printfn "Movement vector: %A" dv
printfn "New center of mass: %A" (center (Curve.toList curve2))

display("My third window", curve2, Color.Black, winSize)
