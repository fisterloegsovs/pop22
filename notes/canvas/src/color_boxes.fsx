//
// Draw 4 colored boxes, which rotation clockwise and counter-clockwise when the user presses the arrow keys.
//
#r "nuget:DIKU.Canvas, 1.0.0-alpha2"

open Canvas

type state =
    | RedStart
    | GreenStart
    | BlueStart
    | YellowStart

let cycleState s =
    match s with
        RedStart -> GreenStart
        | GreenStart -> BlueStart
        | BlueStart -> YellowStart
        | YellowStart -> RedStart

let cycleStateBackwards s =
    match s with
        RedStart -> YellowStart
        | GreenStart -> RedStart
        | BlueStart -> GreenStart
        | YellowStart -> BlueStart

let getPalette (s:state) : (color * color * color * color) =
    match s with
        | RedStart -> (red,green,blue,yellow)
        | GreenStart -> (green,blue,yellow,red)
        | BlueStart -> (blue,yellow,red,green)
        | YellowStart -> (yellow,red,green,blue)

let draw w h (s:state) =
  let C = create w h
  let half = w / 2
  let (c1,c2,c3,c4) = getPalette s
  do setFillBox C c1 (0, 0) (half-1,half-1)
  do setFillBox C c2 (0,half-1) (half-1,h)
  do setFillBox C c3 (half,half) (w,h)
  do setFillBox C c4 (half,0) (w,half-1)  
  C

let react (s:state) (k:key) : state option =
    match getKey k with
        | LeftArrow ->
            do printfn "Pressed left-arrow!"
            Some (cycleStateBackwards s)

        | RightArrow ->
            do printfn "Pressed right-arrow!"
            Some (cycleState s)
        | _ -> None

let width = 600
let height = width

do runApp "ColorBoxes" width height draw react RedStart
