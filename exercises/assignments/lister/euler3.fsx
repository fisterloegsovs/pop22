let rec primes lst n m =
  let nIsPrime =  List.forall (fun i -> n%i <> 0) lst
  let updatedLst =
    if nIsPrime then n::lst
    else lst
  if n = m then
    updatedLst
  else
    primes updatedLst (n+1) m
let largestPrime n = primes [] 2 n |> List.head
let n = 600851475143UL;
let largestFactor = n |> float |> System.Math.Sqrt |> int |> largestPrime
printfn "The largest prime factor of %A is %A" n largestFactor

