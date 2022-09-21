//
// Simple turtle graphics
//
#r "nuget:diku.canvas, 1.0.1"
open Canvas

/// <summary>
/// Turtle commands for generating a fractal tree and return to the origin.
/// </summary>
/// <param sz>The size of the tree</param>
/// <returns>A list of turtle commands</returns>
let rec tree (sz: int) : Canvas.turtleCmd list = 
   if sz < 5 then 
     [Move sz; PenUp; Move (-sz); PenDown]
   else 
     [Move (sz/3); Turn -30] 
     @ tree (sz*2/3) 
     @ [Turn 30; Move (sz/6); Turn 25] 
     @ tree (sz/2) 
     @ [Turn -25; Move (sz/3); Turn 25] 
     @ tree (sz/2) 
     @ [Turn -25; Move (sz/6)]
     @ [PenUp; Move (-sz/3); Move (-sz/6); Move (-sz/3); Move (-sz/6); PenDown]

let w = 600
let h = w
let sz = 100
turtleDraw (w,h) "Tree" (tree sz)
