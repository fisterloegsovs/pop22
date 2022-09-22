let lst = List.fold (fun acc elm -> acc @ [elm]) [] [1..10]
printfn "%A" lst
