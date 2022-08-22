let reverseApply (x:'a) (f:'a -> 'b) : 'b =
  f x

let applylist (fs: ('a -> 'b) list) (x:'a) : 'b list =
  List.map (reverseApply x) fs

let res = applylist [(fun x -> x+2); (fun x -> x*2); (fun x -> x-2)] 8 = [10;16;6]
do printf "%b\n" res
