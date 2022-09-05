#r "nuget:DIKU.Canvas, 1.0"

open Canvas

let w = 600;
let h = 400;
let C = create w h
do setLine C black (0,0) (w-1,h-1)
do show C "My First Canvas"
