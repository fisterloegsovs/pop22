//
// Draw a 'squared' spiral
//
#r "../bin/Debug/net6.0/canvas.dll"

open Canvas

let w = 400;
let h = 600;
let C = create w h
do setLine C red (0,0) (0,h-1)
do setLine C green (0,h-1) (w-1,h-1)
do setLine C blue (w-1,h-1) (w-1,0)
do setLine C yellow (w-1,0) (0,0)
let cw = w/2
let ch = h/2
let bw = w/4
let bh = h/4
do setLine C lightgrey (0,0) (w-1,h-1)
do setLine C lightgrey (0,h-1) (w-1,0)
do setFillBox C black (cw-bw,ch-bh) (cw+bw,ch+bh)
do show C "My First Canvas"
