let lst = List.init 50000 (fun x -> x)
let sum = List.fold (+) 0 lst
do printf "%d\n" sum
