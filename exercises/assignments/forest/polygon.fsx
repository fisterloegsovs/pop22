//
// Simple turtle graphics
//
#r "nuget:diku.canvas, 1.0.1"
open Canvas

type move = int*int // a pair of turn and move

let rec fromMoveRec (lst: move list) : turtleCmd list =
  match lst with
    [] -> []
    | (dir,dist)::rst -> Turn dir::Move dist::fromMoveRec rst

let fromMoveMap (lst: move list) : turtleCmd list =
  List.map (fun (dir,dist) -> [Turn dir;Move dist]) lst
  |> List.concat

let fromMoveFold  (lst: move list) : turtleCmd list =
  List.fold (fun acc (dir,dist) -> Move dist::Turn dir::acc) [] lst
  |> List.rev

let fromMoveFoldBack  (lst: move list) : turtleCmd list =
  List.foldBack (fun (dir,dist) acc -> Turn dir::Move dist::acc) lst []

let lst = [(10,30);(-5,127);(20,90)]
let tCmdRec = fromMoveRec lst
let tCmdMap = fromMoveMap lst
let tCmdFold = fromMoveFold lst
let tCmdFoldBack = fromMoveFoldBack lst
printfn "%A\n%A\n%A\n%A\n%A" lst tCmdRec tCmdMap tCmdFold tCmdFoldBack