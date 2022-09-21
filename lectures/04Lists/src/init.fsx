let rec init (i: int) (m: int) (f: int -> 'T): 'T list =
  if i >= m then []
  else (f i)::(init (i+1) m f)

let lst = init 0 4 (fun i -> i*i)
