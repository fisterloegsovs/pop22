let rec fact = function
  | 0 -> 1
  | x -> x*(fact (x-1))

let impFact x =
  let mutable prod = 1;
  let mutable xn = x;
  while xn > 0 do
    prod <- prod*xn
    xn <- xn - 1
  prod

printfn "fact 10 = %d" (fact 10)
printfn "impFact 10 = %d" (impFact 10)
