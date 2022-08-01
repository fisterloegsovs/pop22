let lst = [[2]; [6; 4]; [1]]
let res = List.fold (fun acc elm -> acc @ elm) [] lst
printfn "collect %A = %A" lst res
