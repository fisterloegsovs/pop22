let rec fact = function
  | 0 -> 1
  | x -> x*(fact (x-1))
printfn "%d" (fact 6)
