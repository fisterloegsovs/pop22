let rec largestPrimeLstAlt lst n m =
  let nIsPrime =  List.forall (fun i -> n%i <> 0) lst
  let updatedListOfPrimes =
    if nIsPrime then n::lst
    else lst
  if n = m then
    updatedListOfPrimes
  else
    largestPrimeLstAlt updatedListOfPrimes (n+1) m
let rec primesLst lst =
  match lst with
    a::b::rst ->
      a::(primesLst (b::(List.filter (fun i -> i % b <> 0) rst)))
    | _ -> lst
let rec largestPrimeLst lst =
  match lst with
    a::[] ->
      a
    | a::b::rst ->
      largestPrimeLst (b::(List.filter (fun i -> i % b <> 0) rst))
    | _ -> 1
let primes n = primesLst [2..n]
let largestPrime n = largestPrimeLst [2..n]
let largestPrimeAlt n = largestPrimeLstAlt [] 2 n |> List.head
let l = largestPrime (int (System.Math.Sqrt(600851475143.0)))
printfn "%A" l

