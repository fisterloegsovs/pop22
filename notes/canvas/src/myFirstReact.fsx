#r "nuget:DIKU.Canvas, 1.0.0-alpha2"

open Canvas

type state = color

let draw w h (s:state) =
  let C = Canvas.create w h
  setFillBox C s (0,0) (w-1,h-1)
  C

let react (s:state) (k:Canvas.key) : state option =
    match getKey k with
        | LeftArrow ->
            printfn "Going red!"
            Some red
        | RightArrow ->
            printfn "Going blue!"
            Some blue
        | _ -> None

do runApp "ColorTest" 600 600 draw react red
