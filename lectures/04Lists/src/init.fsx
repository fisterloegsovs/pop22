let rec init ((i,m): int*int) (f: int -> 'T) (lst: 'T list) : 'T list =
  if i >= m then []
  else (f i)::(init (i+1,m) f lst)

let lst = init (0,4) (fun i -> i*i) []
