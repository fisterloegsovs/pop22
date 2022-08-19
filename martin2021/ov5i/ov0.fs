let oneToNAlt (n:int) : int list =
  let rec loop i = if i > n then []
                   else i :: loop (i+1)
  in loop 1

let oneToN (n:int) : int list =
  List.init n (fun x -> x+1)

let v = oneToN 5

let res = v = [1;2;3;4;5]
do printf "%b\n" res
