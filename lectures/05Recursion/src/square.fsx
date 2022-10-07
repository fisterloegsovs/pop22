let rec square (lst: int list) : int list =
  match lst with
    [] -> []
    | e::rst -> e*e::square rst

let lst = [0..3]
printfn "%A^2 = %A" lst (square lst)
