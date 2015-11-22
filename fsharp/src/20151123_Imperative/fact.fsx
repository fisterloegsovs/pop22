let rec fact = function
  | 0 -> 1
  | x -> x*(fact (x-1))

printfn "fact 10 = %d" (fact 10)
