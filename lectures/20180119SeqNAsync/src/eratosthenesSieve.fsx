let s = Seq.initInfinite (fun i -> i+2)
let sieve n = Seq.filter (fun v -> v%n <> 0)
let primes = Seq.unfold (fun s -> let h = Seq.head s in Some(h, sieve h (Seq.tail s))) s
for i = 0 to 299 do printf "%d " (Seq.item i primes)
printfn "";;
