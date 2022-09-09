#r "nuget:DIKU.Canvas, 1.0"
open System
open Canvas

let add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
let mul (x,y) a = (a*x,a*y)
let dot (x1,y1) (x2,y2) = x1*x2+y1*y2
let rot (x,y) a = (x*Math.Cos a - y*Math.Sin a, x*Math.Sin a + y*Math.Cos a)
let toInt (x,y) = (int x, int y)

let setVector C col v p = setLine C col (toInt p) (toInt (add v p))


(
  let v = (1.0,2.3)
  let w = (-3.5,2.2)
  let a = 4.0

  printfn "v = %A, w = %A, a = %A" v w a
  printfn "v + w = %A" (add v w)
  printfn "av = %A" (mul v a)
  printfn "R (1,0) = %A" (rot (1,0) (Math.PI/2.0))
)

type state = float
(
  let draw w h (s:state) =
    let rec fan C col u a n =
      match n with 
        0 -> ()
        | _ ->
          let v = rot u (s+a*float n)
          setVector C col v (300.0,300.0)
          fan C col u a (n-1)
    let n = 36
    let da = 2.0*Math.PI/float n
    let C = create w h
    let u = (0.0,float (h/2))
    fan C black u da n
    C

  let react (s:state) (k:Canvas.key) : state option =
      match getKey k with
          | LeftArrow -> Some (s-0.01)
          | RightArrow -> Some (s+0.01)
          | _ -> None

  do runApp "ColorTest" 600 600 draw react 0.0
)