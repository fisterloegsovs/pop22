// Code from Hansen and Rischel: Functional Programming using F#     16/12 2012
// Chapter 7: Modules

// From Section 7.9 Example: piecewise linear plane curves 
// Hilbert curves and Displaying curves

//                                                          Revised  21/4 2015
// Windows version: 
// Only the last line of the function display differs from the Mono version

// reference the Curve library
#r @"c:\mrh\Forskning\Cambridge\HomePage\Programs\Chapter7\Curve.dll";;


// Display of curves
open System.Drawing
open System.Windows.Forms
Application.EnableVisualStyles();;

let winSize = Size(450,300);; // Initial window size in pixels

let display(title: string,(c: Curve.Curve,pw: int,ph: int)) =
    let f(x,y) = Point(int(round x), ph - int(round y))
    let clst = Curve.toList c
    let Ptlst = List.map f clst
    let pArr = Array.ofList Ptlst

    let pen = new Pen(Color.Black)
    let draw(g:Graphics) = g.DrawLines(pen,pArr)

    let panel = new Panel(Dock=DockStyle.Fill)
    panel.Paint.Add(fun e -> draw(e.Graphics))

    let win = new Form(Text=title,Size=winSize,AutoScroll=true,
                       AutoScrollMinSize=Size(pw,ph))
    win.Controls.Add(panel)
    win.Show();;


let adjust(c:Curve.Curve, a: float) =
    let c1 = a * c --> (10.0, 10.0)
    let (_,(maxX,maxY)) = Curve.boundingBox c1
    let pw = int(round maxX) + 20
    let ph = int(round maxY) + 20
    (c1,pw,ph);;


// Generation of hilbert curves
let h0 = Curve.point (0.0,0.0);;

// Generates the hilbert curve h^(n+1) from h^n 
let hilbert hn =
    let w  = Curve.width hn
    let h  = Curve.height hn
    let c0 = hn >< 0.0
    let c1 = c0 |^ -90
    let c2 = hn --> (0.0, w + 1.0)
    let c3 = hn --> (h + 1.0, w + 1.0)
    let c4 = (c0 |^ 90) --> (h + h + 1.0, w)
    c1 + c2 + c3 + c4;;

let h1 = hilbert h0;;
let h2 = hilbert h1;;
let h3 = hilbert h2;;
let h4 = hilbert h3;;
let h5 = hilbert h4;;
let h6 = hilbert h5;;
display("Hilbert Curve 6", adjust(h6, 10.0));;