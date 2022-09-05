#r "nuget:DIKU.Canvas, 1.0"
open Canvas

type state = int

let draw w h (s:state) =
  let C = create w h
  let left = w / 4
  let right = 3*left
  do setFillBox C blue (left+s, left) (right+s,right)
  C

let react (s:state) (k:key) : state option =
    match getKey k with
        LeftArrow -> Some (s-5)
        | RightArrow -> Some (s + 5)
        | _ -> None

do runApp "ColorBoxes" 600 600 draw react 0
