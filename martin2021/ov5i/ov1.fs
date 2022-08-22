let rec multiplicityAlt (x:int) (xs:int list) : int =
  match xs with
    [] -> 0
  | y :: xs -> ((if x = y then 1 else 0) + multiplicityAlt x xs)

let multiplicity (x:int) (xs:int list) : int =
  List.fold (fun acc y -> acc + (if x = y then 1 else 0)) 0 xs

let v = multiplicity 4 [45;34;53;5;667;2;23;4;45;3;2;4;23;23;56;4;4567]

let res = v = 3
do printf "%b\n" res;
