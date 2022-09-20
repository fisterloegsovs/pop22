//
// Simple turtle graphics
//
#r "nuget:diku.canvas, 1.0.1"
open Canvas

let translate theta delta lst =
  [PenUp; Turn theta; Move delta; Turn -theta; PenDown] 
  @ lst 
  @ [PenUp; Turn theta; Move -delta; Turn -theta; PenDown] 

let rotate theta lst =
  Turn theta :: lst @ [Turn -theta]

let rec scale s lst =
  let scaleIt elm = 
    match elm with
      Move delta -> Move (int (s*float delta))
      | _ -> elm
  List.map scaleIt lst

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
      let theta = rnd.Next 360
      let delta = rnd.Next maxStep
      let s = 0.1+2.9*rnd.NextDouble ()
      let phi = rnd.Next 40 - 20
      let newTree = tree sz |> translate 0 -50 |> rotate phi |> scale s |> translate theta delta
      newTree @ randomTree maxStep sz (n-1)

let w = 600
let h = w
let maxStep = w/4
let sz = 100
let nTrees = 20;
let pic = randomTree maxStep sz nTrees

do turtleDraw (w,h) "Forest" pic 
