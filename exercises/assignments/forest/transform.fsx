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
  let update elm = 
    match elm with
      Move delta -> Move (int (s*float delta))
      | _ -> elm
  List.map update lst

type pos = float*float
type posDir = pos*int
let toXY (lst: turtleCmd list) : (posDir list) =
  let update (acc: posDir list) (elm: turtleCmd) : posDir list =
    let (p,d) = acc.Head 
    match elm with
      Move delta->
        let r = float delta
        let t = float d
        let newX = (fst p) + r*cos (t*System.Math.PI/180.0)
        let newY = (snd p) + r*sin (t*System.Math.PI/180.0)
        ((newX,newY),d)::acc
      | Turn theta -> (p,d+theta)::acc
      | _ -> (p,d)::acc
  List.fold update [((0.0,0.0),0)] lst
  |> List.rev

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
      let newTree = tree sz |> scale s |> translate 0 -150 |> rotate phi |> translate theta delta
      newTree @ randomTree maxStep sz (n-1)

let w = 600
let h = w
let maxStep = w/4
let sz = 100
let nTrees = 20;
let pic = randomTree maxStep sz nTrees
let xy = sz |> tree |> toXY
printfn "%A %A" xy[0] xy[xy.Length-1]

do turtleDraw (w,h) "Forest" pic 
