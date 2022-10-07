let lst = List.map (fun j -> List.map (fun i -> (i,j)) [0..3]) [0..4] |> List.concat
printfn "%A" lst
