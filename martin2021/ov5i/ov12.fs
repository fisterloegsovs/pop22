let reverseArrayD (a:'a array) : unit =
  let mutable i = 0
  in while (i < a.Length / 2) do
       let tmp = a.[i]
       let j = a.Length-i-1
       a.[i] <- a.[j]
       a.[j] <- tmp
       i <- i + 1

let a = [|1..5|]
let b = [|1..6|]
do reverseArrayD a
do reverseArrayD b
let res = Array.toList a = [5 .. -1 .. 1] && Array.toList b = [6 .. -1 .. 1]
do printf "%b\n" res
