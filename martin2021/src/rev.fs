let badrev (arr: int[]) : unit =
  for i in [0..arr.Length-1] do
    arr.[i] <- arr.[arr.Length-i-1]

let arr = [|1;2;3;4|]
do badrev arr
do printf "%A\n" arr

let rev (arr: int[]) : unit =
  for i in [0..arr.Length/2-1] do
    let tmp = arr.[i]
    do arr.[i] <- arr.[arr.Length-i-1]
    do arr.[arr.Length-i-1] <- tmp;;

let arr2 = [|1;2;3;4|]
do rev arr2
do printf "%A\n" arr2

let arr3 = [|1;2;8;3;4|]
do rev arr3
do printf "%A\n" arr3
