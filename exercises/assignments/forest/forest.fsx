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

let rnd = System.Random()

let rec randomTree maxStep sz n =
  match n with
    0 -> []
    | _ -> 
      let a = rnd.Next 360
      let s = rnd.Next maxStep
      [PenUp; Turn a; Move s; Turn -a; PenDown] 
      @ tree sz 
      @ [PenUp; Turn a; Move -s; Turn -a; PenDown] 
      @ randomTree maxStep sz (n-1)

let w = 600
let h = w
let maxStep = w/4
let sz = 100
let nTrees = 20;
let pic = Move -100 :: randomTree maxStep sz nTrees

do turtleDraw (w,h) "Forest" pic 
