let allPair (a: 'a list) (b: 'b list) : ('a*'b) list =
  List.map (fun i -> (List.map (fun j -> (i,j)) b)) a |> List.concat

printfn "%A" (allPair [0..3] [0..3])
