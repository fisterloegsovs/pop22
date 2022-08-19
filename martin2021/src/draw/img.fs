//
// Library for Functional images
// Inspired by Conal Elliott's paper on the topic from the
// book "The Fun of Programming".
// Martin Elsman (c), MIT License
//

module Img

  open System.Drawing

  // read a bitmap file (jpg,png,...)
  let fromFile (fname : string) : Bitmap =
    new Bitmap(fname)

  // save a bitmap as a png file
  let toPngFile (fname : string) (b: Bitmap) : unit =
    b.Save(fname, Imaging.ImageFormat.Png) |> ignore

  // Some type abbreviations
  type frac = float                          // floats in interval [0;1]
  type color = frac * frac * frac * frac     // alpha,red,green,blue

  let toSColor ((a,r,g,b):color) : System.Drawing.Color =
    Color.FromArgb (int(255.0*a),int(255.0*r),int(255.0*g),int(255.0*b))

  let fromSColor (c: System.Drawing.Color) : color =
    let w = c.ToArgb()
    let a = (w >>> 24) &&& 255
    let r = (w >>> 16) &&& 255
    let g = (w >>> 8) &&& 255
    let b = w &&& 255
    let conv x = float x / 255.0
    in (conv a,conv r,conv g,conv b)

  let greyifyColor ((a,r,g,b):color) : color =
    let v = (r + g + b) / 3.0
    in (a,v,v,v)

  type point = float * float
  type 'a image = point -> 'a
  type region = bool image

  let vstrip : region =
    fun (x,y) -> abs x <= 0.5

  let even x = x % 2 = 0

  let floori x = int(floor x)

  let checker : region =
    fun (x,y) -> even(floori x + floori y)

  let distO (x,y) = sqrt(x*x+y*y)

  let altRings : region =
    even << floori << distO

  type polar_point = float * float

  let pi = System.Math.PI
  let fromPolar ((r,t):polar_point) : point = (r*cos t, r*sin t)
  let toPolar ((x,y):point) : polar_point = (distO (x,y), atan2 y x)

  let polarChecker n : region =
    let sc (r,t) = (r, t * float n / pi)
    in checker << sc << toPolar

  let wavDist : frac image =
    fun p -> (1.0 + cos (pi * distO p)) / 2.0

  let imgToBitmap (bmp:System.Drawing.Bitmap) (width:float) (img:color image): unit =
    let (w,h) = (bmp.Width, bmp.Height)
    for x in [0..w-1] do
      for y in [0..h-1] do
        let p_x = width * float (x - w/2) / float w
        let p_y = width * float (y - h/2) / float h
        let c = img (p_x,p_y)
        let sc = toSColor c
        in bmp.SetPixel (x,y,sc)

  let boolToColor b =
    if b then (1.0,0.0,0.0,0.0)
    else (1.0,1.0,1.0,1.0)

  let fracToColor (f:frac) = (1.0,f,f,f)
