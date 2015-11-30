/// Open a window using winforms in Mono. The program opens a window
/// and draws a bitmap of black and white squares. Uses timer module
/// to draw a new bitmap every second.
///
/// How to compile:
/// <code>
/// fsharpc pixels2.fsx
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/11/29

let coordArray = Collections.Array2D.init<int> 10 4 (fun i j -> -1) // A circular array, -1 implies not defined
let mutable pointer = 0; // Points to the head

let display (aTitle : string, aWidth : int, aHeight : int, forgroundColor : System.Drawing.Color, backgroundColor : System.Drawing.Color) =

    let sz = new System.Drawing.Size (aWidth, aHeight)
    let draw (g:System.Drawing.Graphics) =
      let length = Collections.Array2D.length1 coordArray

      let drawLines (g:System.Drawing.Graphics, aColor : System.Drawing.Color) = 
        let backgroundPen = new System.Drawing.Pen (aColor)
        for i = 0 to length - 1 do
          let current = (pointer + i) % length
          if not (coordArray.[current,1] = -1) then
            let pArray = [|System.Drawing.Point (coordArray.[current,0], coordArray.[current,1]); System.Drawing.Point (coordArray.[current,2], coordArray.[current,3])|]
            g.DrawLines (backgroundPen, pArray)

      // delete all lines, update last line, and redraw
      drawLines(g, backgroundColor)
      let (x1, y1, x2, y2) = (coordArray.[pointer,0], coordArray.[pointer,1], coordArray.[pointer,2], coordArray.[pointer,3])
      pointer <- (pointer - 1) % length
      drawLines(g, forgroundColor)

    let panel = new System.Windows.Forms.Panel (Dock = System.Windows.Forms.DockStyle.Fill)
    let timer = new System.Windows.Forms.Timer (Interval = 1000, Enabled = true)  
    let winSize = System.Drawing.Size (aWidth, aHeight)
    let win = new System.Windows.Forms.Form (Text = aTitle, ClientSize = winSize, MaximizeBox = false, MinimizeBox = false)

    panel.Paint.Add (fun e -> draw (e.Graphics))
    timer.Tick.Add (fun e -> panel.Refresh ())
    win.Controls.Add panel
    System.Windows.Forms.Application.Run win

let generateRandomImage (aWidth : int, aHeight : int) =
  let rnd = System.Random ()    
  Collections.Array2D.init<bool> aWidth aHeight (fun i j -> (rnd.Next(2) > 0))

let width = 30;
let height = 55;
let forgroundColor = System.Drawing.Color.Black
let backgroundColor = System.Drawing.Color.White

display ("A bitmap", width, height, forgroundColor, backgroundColor)
