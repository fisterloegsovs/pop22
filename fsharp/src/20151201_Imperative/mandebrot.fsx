/// Open a window using winforms in Mono. The program opens a window
/// and draws a bitmap of black and white squares.
///
/// How to compile:
/// <code>
/// fsharpc pixels.fsx
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/11/29

let display(aTitle: string, topLeft : float*float, bottomRight : float*float, width : int, height : int) =
  let iterMax = 512
  let (CxMin,CyMin) = topLeft
  let (CxMax,CyMax) = bottomRight
  
  let pixelWidth = 1
  let pixeHeight = 1
  let sz = new System.Drawing.Size (pixelWidth, pixeHeight)

  let draw (g:System.Drawing.Graphics) =
    for m = 0 to width do
      let Cx = CxMin + (CxMax-CxMin) * (float m) / ((float width) - 1.0);
      for n = 0 to height do
        let Cy = CyMin + (CyMax-CyMin) * (float n) / ((float height) - 1.0);

        let mutable Zx = 0.0
        let mutable Zy = 0.0
        let mutable iter = 0
        while (Zx*Zx+Zy*Zy <= 4.0) && (iter < iterMax) do
          let Tx = Zx*Zx-Zy*Zy + Cx
          let Ty = 2.0*Zx*Zy + Cy
          Zx <- Tx
          Zy <- Ty
          iter <- iter+1
        let I = 255 * iter / iterMax
        //let I = 255 * (int (log((float iter) + 1.0) / log((float iterMax) + 1.0)))
        let aColor = System.Drawing.Color.FromArgb(I,I,I)
        let brush = new System.Drawing.SolidBrush (aColor)
        let p = System.Drawing.Point ( m*pixelWidth, n*pixeHeight )
        let rect = new System.Drawing.Rectangle (p, sz)
        g.FillRectangle (brush, rect)
        
  let panel = new System.Windows.Forms.Panel (Dock = System.Windows.Forms.DockStyle.Fill)
  let winSize = System.Drawing.Size (width * pixelWidth, height * pixeHeight)
  let win = new System.Windows.Forms.Form (Text = aTitle, ClientSize = winSize, MaximizeBox = false, MinimizeBox = false)

  panel.Paint.Add (fun e -> draw (e.Graphics))
  win.Controls.Add panel
  System.Windows.Forms.Application.Run win

let width = 512;
let height = 512;

//let CxMin = -1.9
//let CxMax = 0.6
//let CyMin = -1.25;
//let CyMax = 1.25;

//let CxMin = -1.5
//let CxMax = -1.3
//let CyMin = -0.1
//let CyMax = 0.1

let CxMin = -1.485
let CxMax = -1.472
let CyMin = -0.005
let CyMax = 0.005

display("A bitmap", (CxMin,CyMin), (CxMax,CyMax), width, height)
