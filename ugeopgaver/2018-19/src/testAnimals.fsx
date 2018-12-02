let m = animals.moose(11);
let w = animals.wolf(12, 5);
let isle = animals.environment(10, 6, 5, 3, 5, 6, true)

printfn "%A %A" m w
printfn "%A" isle

let mutable board = isle.board
let printMoose (e : animals.moose) = printfn "%c: %A %A" e.symbol e.position e.reproduction
let printWolf (e : animals.wolf) = printfn "%c: %A %A %A" e.symbol e.position e.reproduction e.hunger
List.iter printMoose (board.moose)
List.iter printWolf (board.wolves)

for i = 1 to 100 do
  printfn "-------------------------" 
  isle.tick()
  board <- isle.board
  List.iter printMoose (board.moose)
  List.iter printWolf (board.wolves)
  printfn "%A" isle
