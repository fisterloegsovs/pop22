let evens lst =
  List.filter (fun e -> (e % 2) = 0) lst

let lst = [1; 2; 1; 3; 5; 3; 1]
printfn "evens %A = %A" lst (evens lst)
