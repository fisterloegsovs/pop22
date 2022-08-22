let squares (n:int) : int array =
  if n < 0 then
    failwith "squares applied to a negative number"
  else Array.init n (fun x -> (x+1)*(x+1))

let res = Array.toList(squares 4) = [1;4;9;16]
do printf "%b\n" res
