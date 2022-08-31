//
// Draw a 'squared' spiral
//
#r "../bin/Debug/net6.0/canvas.dll"

open Canvas

type state = int

let draw w h (s:state) =
    let C = create w h

    let speed = 5;
    let cw = w/2
    let ch = h/2
    let bw = w/4
    let bh = h/4
    do setFillBox C black (cw-bw-s*speed,ch-bh) (cw+bw-s*speed,ch+bh)
    do setLine C lightgrey (0,0) (cw-bw-s*speed,ch-bh)
    do setLine C lightgrey (cw+bw-s*speed,ch+bh) (w-1,h-1)
    do setLine C lightgrey (w-1,0) (cw+bw-s*speed,ch-bh)
    do setLine C lightgrey (0,h-1) (cw-bw-s*speed,ch+bh)
    C

let react (s:state) (k:key) : state option =
    match getKey k with
        | Space ->
            Some 0
        | LeftArrow ->
            Some (s+1)

        | RightArrow ->
            Some (s-1)
        | _ -> None

let width = 600
let height = width

do runApp "Game" width height draw react 0
