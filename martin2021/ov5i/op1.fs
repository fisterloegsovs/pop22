let concat (xs: 'a list list) : 'a list =
  List.foldBack (fun x acc -> x @ acc) xs []

let res = concat [[2;3];[5];[4;7;3]] = [2;3;5;4;7;3]
do printf "%b\n" res
