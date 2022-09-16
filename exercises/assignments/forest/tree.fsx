//
// Simple turtle graphics
//
#r "nuget:diku.canvas, 1.0.1"
open Canvas

let rec tree sz =
   if sz < 5 then 
     [Move sz; PenUp; Move (-sz); PenDown]
   else 
     [Move (sz/3); Turn -30] 
     @ tree (sz*2/3) 
     @ [Turn 30; Move (sz/6); Turn 25] 
     @ tree (sz/2) 
     @ [Turn -25; Move (sz/3); Turn 25] 
     @ tree (sz/2) 
     @ [Turn -25; Move (sz/6); PenUp; Move (-sz/3); Move (-sz/6); Move (-sz/3); Move (-sz/6); PenDown]

let w = 600
let h = w
let sz = 100

turtleDraw (w,h) "Tree" (tree sz)
