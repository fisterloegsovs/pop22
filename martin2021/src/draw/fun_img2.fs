//
// Development of Functional images
// Inspired by Conal Elliott's paper on the topic from the
// book "The Fun of Programming".
// Martin Elsman (c), MIT License
//

module FunImg

  open System.Drawing

  // Some type abbreviations
  type point = float * float          // Points in the plane
  type 'a image = point -> 'a         // Generic image

  type frac = float                   // floats in [0;1]
  type fcolor = frac*frac*frac*frac   // alpha,red,green,blue

  type region = bool image            // region (b/w)
  type cimage = fcolor image          // color images

  type bitmap = System.Drawing.Bitmap

  let toColor ((a,r,g,b):fcolor) : System.Drawing.Color =
    ImgUtil.fromArgb (int(255.0*a),int(255.0*r), int(255.0*g),int(255.0*b))

  let boolToFColor (b:bool) : fcolor =
    if b then (1.0,0.0,0.0,0.0)
    else (1.0,1.0,1.0,1.0)

  let fromColor (c: System.Drawing.Color) : fcolor =
    let w = c.ToArgb()
    let a = (w >>> 24) &&& 255
    let r = (w >>> 16) &&& 255
    let g = (w >>> 8) &&& 255
    let b = w &&& 255
    let conv x = float x / 255.0
    in (conv a,conv r,conv g,conv b)

  let greyifyFunColor ((a,r,g,b):fcolor) : fcolor =
    let v = (r + g + b) / 3.0
    in (a,v,v,v)

  let vstrip : region =
    fun (x,y) -> abs x <= 0.5

  let even x = x % 2 = 0

  let floori x = int(floor x)

  let checker : region =
    fun (x,y) -> even(floori x + floori y)

  let distO (x,y) = sqrt(x*x+y*y)

  let altRings : region =
    even << floori << distO

  let pi = System.Math.PI

  type polar_point = float * float

  let fromPolar ((r,t):polar_point) : point = (r*cos t, r*sin t)
  let toPolar ((x,y):point) : polar_point = (distO (x,y), atan2 y x)

  let polarChecker n : region =
    let sc (r,t) = (r, t * float n / pi)
    in checker << sc << toPolar

  let wavDist : frac image =
    fun p -> (1.0 + cos (pi * distO p)) / 2.0

  let toBitmap (width:float) (img:cimage) w h : bitmap =
    let bmp = ImgUtil.mk w h
    for x in [0..w-1] do
      for y in [0..h-1] do
        let p_x = width * float (x - w/2) / float w
        let p_y = width * float (y - h/2) / float h
        let fc = img (p_x,p_y)   // function call!
        let c = toColor fc       // convert color
        in ImgUtil.setPixel c (x,y) bmp
    bmp

  let fracToFColor (f:frac) = (1.0,f,f,f)

  let interpolC w ((r1,g1,b1,a1):fcolor) ((r2,g2,b2,a2):fcolor) : fcolor =
    let h x1 x2 = w * x1 + (1.0-w)*x2
    in (h r1 r2, h g1 g2, h b1 b2, h a1 a2)

  let bilerpC w h c1 c2 c3 c4 =
    let c_low = interpolC w c1 c2
    let c_high = interpolC w c3 c4
    in interpolC h c_low c_high

  let overC ((a1,r1,g1,b1):fcolor) ((a2,r2,g2,b2):fcolor) : fcolor =
    let h x1 x2 = x1 + (1.0-a1)*x2
    in (h a1 a2,h r1 r2,h g1 g2,h b1 b2)

  let lift1 h f1 p = h (f1 p)
  let lift2 h f1 f2 p = h (f1 p) (f2 p)
  let lift3 h f1 f2 f3 p = h (f1 p) (f2 p) (f3 p)

  let over : cimage -> cimage -> cimage =
    fun x y -> lift2 overC x y

  let cond : bool image -> 'a image -> 'a image -> 'a image =
    fun x y c -> lift3 (fun a b c -> if a then b else c) x y c

  let interpolI : frac image -> cimage -> cimage -> cimage =
    fun x y z -> lift3 interpolC x y z

  let constant a (p:point) = a
  let blueI = constant (1.0,0.0,0.0,1.0)
  let redI = constant (1.0,1.0,0.0,0.0)
  let greenI = constant (1.0,0.0,1.0,0.0)
  let yellowI = constant (1.0,0.0,1.0,1.0)

  let rbRings = interpolI wavDist redI blueI
  let mystique : cimage =
    interpolI (constant 0.2) (boolToFColor<<checker) rbRings

  let (<<>>) : float -> float -> bool = fun (x:float) y -> abs(x-y) < 0.05
  let f : region = fun (x,y) -> y <<>> (x*x*x - 2.0*x*x + 1.5)

  open ImgUtil

  do toPngFile "f.png" (toBitmap 4.0 (boolToFColor << f) 600 400)

  let invY (f : 'a image) : 'a image = fun (x,y) -> f (x,-y)

  do toPngFile "finv.png" (toBitmap 4.0 (invY(boolToFColor << f)) 600 400)

  let (<||>) : region -> region -> region =
    fun r1 r2 -> fun p -> r1 p || r2 p

  let coord : region =
    fun (x,y) -> x <<>> 0.0 || y <<>> 0.0

  let circ : region =                          // $1 = x^2 + y^2$
    fun (x,y) -> 1.0 <<>> (x*x + y*y)

  let save600x400s sz f img =
    toPngFile f (toBitmap sz img 600 400)

  let save600x400 = save600x400s 4.0

  do save600x400 "f.png" (boolToFColor << f)
  do save600x400 "finv.png" (invY(boolToFColor << f))

  do save600x400 "circ.png" (boolToFColor << circ)
  do save600x400 "coord.png" (boolToFColor << coord)
  do save600x400 "ccirc.png" (boolToFColor << (coord <||> circ))

  do save600x400s 7.0 "checker.png" (boolToFColor << checker)
  do save600x400s 7.0 "altRings.png" (boolToFColor << altRings)

  do save600x400s 7.0 "polarChecker3.png" (boolToFColor << polarChecker 3)
  do save600x400s 7.0 "polarChecker5.png" (boolToFColor << polarChecker 5)
  do save600x400s 7.0 "wavDist7.png" (fracToFColor << wavDist)
  do save600x400s 9.0 "wavDist9.png" (fracToFColor << wavDist)

  do save600x400s 7.0 "rbRings.png" rbRings
  do save600x400s 7.0 "mystique.png" mystique
