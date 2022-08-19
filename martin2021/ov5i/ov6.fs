let evens (xs:int list) : int list =
  List.filter (fun x -> x % 2 = 0) xs

let res = evens [3;5;2;4;1;6] = [2;4;6]
do printf "%b\n" res
