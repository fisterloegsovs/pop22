let lst = List.fold (fun acc elm -> elm :: acc) [] [1..10]
printfn "%A" lst
