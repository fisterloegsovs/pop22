let lst = [[2]; [6; 4]; [1]]
let res = List.collect id lst
printfn "collect %A = %A" lst res
