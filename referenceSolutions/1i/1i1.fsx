(*
   Based on `color_boxes.fsx` from Absalon, found in
   files/src/Canvas/color_boxes.fsx
*)

#r "nuget:DIKU.Canvas, 1.0.1"

open Canvas

type state = int * int

let draw width height (s:state) =
  let canvas = create width height
  // The state is a tuple with the center-coords for the box
  // Unpack the state into x and y coordinate
  let (boxX, boxY) = s
  let halfBoxSize = width / 4
  // We assume the box is width / 2, since it is not specified in the assignment
  let topLeft = boxX - halfBoxSize, boxY - halfBoxSize
  let bottomRight = boxX + halfBoxSize, boxY + halfBoxSize
  let topRight = boxX + halfBoxSize, boxY - halfBoxSize
  let bottomLeft = boxX - halfBoxSize, boxY + halfBoxSize
  do setFillBox canvas black topLeft bottomRight
  // Draw some sweet sweet grey lines to each corner
  do setLine canvas lightgrey (0,0) topLeft
  do setLine canvas lightgrey (width,0) topRight
  do setLine canvas lightgrey (0,height) bottomLeft
  do setLine canvas lightgrey (width,height) bottomRight
  canvas

let moveLeft (s:state) =
    let x,y = s
    (x-5,y)

let moveRight (s:state) =
    let x,y = s
    (x+5,y)

let moveUp (s:state) =
    let x,y = s
    (x,y-5)

let moveDown (s:state) =
    let x,y = s
    (x,y+5)


let react (s:state) (k:key) : state option =
    match getKey k with
        | LeftArrow  -> Some (moveLeft s)
        | RightArrow -> Some (moveRight s)
        // The up and down case are not asked for by the assignment
        // But they are a nice and easy addition
        | UpArrow    -> Some (moveUp s)
        | DownArrow  -> Some (moveDown s)
        | _          -> None

let width,height = 600,600
let center = width / 2

do runApp "Move The Box" width height draw react (center,center)
