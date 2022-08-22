(*
 * Bitmap Primitives Helpers
 * Partly inspired by http://fssnip.net/si.
*)
open System.Drawing

// deserialize a bitmap file
let fromFile (name : string) = new Bitmap(name)

// serialize a bitmap as png
let toPngFile (name : string) (bmp: Bitmap) =
    bmp.Save(name, Imaging.ImageFormat.Png) |> ignore
    bmp

let greyifyColor (c : System.Drawing.Color) =
  let w = c.ToArgb()
  let r = (w >>> 16) &&& 255
  let g = (w >>> 8) &&& 255
  let b = w &&& 255
  let v = (r + g + b) / 3
  in Color.FromArgb (v,v,v)

type frac = float
type color = frac * frac * frac * frac
let toSColor ((a,r,g,b):color) : System.Drawing.Color =
  Color.FromArgb (int(255.0*a),int(255.0*r),int(255.0*g),int(255.0*b))

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

let imgToBitmap (width:float) (img:color image): System.Drawing.Bitmap =
  let (w,h) = (640, 480)
  let bmp = new Bitmap(w, h)
  for x in [0..w-1] do
    for y in [0..h-1] do
      let p_x = width * float (x - w/2) / float w
      let p_y = width * float (y - h/2) / float h
      let c = img (p_x,p_y)
      let sc = toSColor c
      in bmp.SetPixel (x,y,sc)
  bmp

let boolToColor b =
  if b then (1.0,0.0,0.0,0.0)
  else (1.0,1.0,1.0,1.0)

let fracToColor f = (1.0,f,f,f)

let img_b = imgToBitmap 10.0 (boolToColor << polarChecker 50)

let img_c = imgToBitmap 10.0 (fracToColor << wavDist)

let _ = toPngFile @"tmp.png" img_c
