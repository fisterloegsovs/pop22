//
// Simple turtle graphics
//
#r "nuget:diku.canvas, 1.0.1"
open Canvas

type move = int*int // a pair of move and turn

let rec fromMoveRec (mLst: move list) : turtleCmd list =
  match mLst with
    [] -> []
    | (dist,dir)::rst -> Turn dir::Move dist::fromMoveRec rst

let fromMoveCollect (mLst: move list) : turtleCmd list =
  List.collect (fun (dist,dir) -> [Turn dir;Move dist]) mLst

let fromMoveFold  (mLst: move list) : turtleCmd list =
  List.fold (fun acc (dist,dir) -> Move dist::Turn dir::acc) [] mLst
  |> List.rev

let fromMoveFoldBack  (mLst: move list) : turtleCmd list =
  List.foldBack (fun (dist,dir) acc -> Turn dir::Move dist::acc) mLst []

let mLst = [(10,30);(-5,127);(20,90)]
let tCmdRec = fromMoveRec mLst
let tCmdCollect = fromMoveCollect mLst
let tCmdFold = fromMoveFold mLst
let tCmdFoldBack = fromMoveFoldBack mLst
printfn "%A\n%A\n%A\n%A\n%A" mLst tCmdRec tCmdCollect tCmdFold tCmdFoldBack