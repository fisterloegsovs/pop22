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
     @ [Turn -25; Move (sz/6); PenUp; Move (-sz/3); Move (-sz/6); Move (-sz/3); Move (-sz/6); PenDown]

let rnd = System.Random()

/// <summary>
/// Turtle commands for generating a fractal tree randomly placed on a canvas
/// and return to the origin
/// </summary>
/// <param sz>The size of the tree</param>
/// <returns>A list of turtle commands</returns>
let rec randomTree (sz: int) : Canvas.turtleCmd list = 
  let a = rnd.Next 360
  let s = rnd.Next 100
  [PenUp; Turn a; Move s; Turn -a; PenDown] 
  @ tree sz 
  @ [PenUp; Turn a; Move -s; Turn -a; PenDown] 

/// <summary>
/// Turtle commands for generating several fractal trees randomly placed on a canvas
/// and return to the origin
/// </summary>
/// <param sz>The size of the tree</param>
/// <param n>The number of the trees to produce commands for</param>
/// <returns>A list of turtle commands</returns>
let rec forest sz n =
  match n with
    0 -> []
    | _ ->  randomTree sz @ forest sz (n-1)

let w = 600
let h = w
let sz = 100
let nTrees = 20;
let pic = Move -100 :: forest sz nTrees

do turtleDraw (w,h) "Forest" pic 
