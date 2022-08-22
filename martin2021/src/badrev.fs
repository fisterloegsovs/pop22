let badrev (arr: int[]) : unit =
  for i in [0..arr.Length-1] do
    arr.[i] <- arr.[arr.Length-i-1]
let arr = [|1;2;3;4|]
do badrev arr
do printf "%A\n" arr
