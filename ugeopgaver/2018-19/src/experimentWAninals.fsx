[<EntryPoint >]
let main args =
  let iterations = if args.Length < 1 then 40 else int args.[0]
  let filename = if args.Length < 2 then None else Some args.[1]
  let boardWidth = if args.Length < 3 then 10 else int args.[2]
  let NMooses = if args.Length < 4 then 30 else int args.[3]
  let mooseRepLen = if args.Length < 5 then 10 else int args.[4]
  let NWolves = if args.Length < 6 then 2 else int args.[5]
  let wolvesRepLen = if args.Length < 7 then 10 else int args.[6]
  let wolvesHungLen = if args.Length < 8 then 4 else int args.[7]

  let isle = animals.environment(boardWidth, NMooses, mooseRepLen, NWolves, wolvesRepLen, wolvesHungLen)
  let file = Option.bind (fun e -> Some (System.IO.File.CreateText e)) filename

  for i = 1 to iterations do
    isle.tick ()
    let board = isle.board
    printfn "%3d: %3d moose and %d wolves" i board.moose.Length board.wolves.Length
    Option.iter (fun (e : System.IO.StreamWriter) -> e.WriteLine (sprintf "%d, %d, %d" i board.moose.Length board.wolves.Length)) file 
  Option.iter (fun (e : System.IO.StreamWriter) -> e.Close ()) file 

  0 // Signals that program terminated successfully
