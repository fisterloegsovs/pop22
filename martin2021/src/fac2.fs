let rec fac2 acc n =
  if n <= 1 then acc
  else fac2 (n*acc) (n-1)

let xs = List.init 10 (fun x -> x + 1)
do printf "%A\n" (List.map (fac2 1) xs)
