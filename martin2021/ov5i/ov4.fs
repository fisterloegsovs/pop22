let reverseApply (x:'a) (f:'a -> 'b) : 'b =
  f x

let res = reverseApply 9 (fun x -> x + 8) = 17
do printf "%b\n" res
