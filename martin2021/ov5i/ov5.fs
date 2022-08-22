// f : int -> (int -> int)
let f (x:int) : int -> int =
  fun y -> x + y

// g : (int -> int) -> int
let g (f: int -> int) : int =
  f 5

let res = (f 5) 8 = 13 && g (fun y -> y+2) = 7
do printf "%b\n" res
