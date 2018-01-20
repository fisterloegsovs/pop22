let isPrime n =
  let sqrtInt = float >> sqrt >> int
  List.forall (fun x -> n % x <> 0) [ 2 .. sqrtInt n ]  

let s = Seq.initInfinite (fun i -> i+2)
let primes = Seq.filter isPrime s
for i = 0 to 299 do printf "%d " (Seq.item i primes)
printfn "";;
