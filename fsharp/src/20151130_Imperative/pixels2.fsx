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

let display (aTitle : string, imageFct : int * int -> bool [ , ], aWidth : int, aHeight : int, aTrueColor : System.Drawing.Color, aFalseColor : System.Drawing.Color) =
    let trueBrush = new System.Drawing.SolidBrush (aTrueColor)
    let falseBrush = new System.Drawing.SolidBrush (aFalseColor)
    let pixelWidth = 10
    let pixeHeight = 10
    let sz = new System.Drawing.Size (aWidth, aHeight)
    let draw (g:System.Drawing.Graphics) =
      let bitMap = imageFct (aWidth, aHeight)
      for i = 0 to aWidth - 1 do
        for j = 0 to aHeight - 1 do
          let p = System.Drawing.Point ( i*pixelWidth, j*pixeHeight )
          let rect = new System.Drawing.Rectangle (p, sz)
          if bitMap.[i,j] then
            g.FillRectangle (trueBrush, rect)
          else
            g.FillRectangle (falseBrush, rect)
    let panel = new System.Windows.Forms.Panel (Dock = System.Windows.Forms.DockStyle.Fill)
    let timer = new System.Windows.Forms.Timer (Interval = 1000, Enabled = true)  
    let winSize = System.Drawing.Size (aWidth * pixelWidth, aHeight * pixeHeight)
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
display ("A bitmap", generateRandomImage, width, height, forgroundColor, backgroundColor)
