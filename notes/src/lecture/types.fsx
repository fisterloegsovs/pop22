let a = 3
let f x = x*x
let g (x : float) = x*x
let h x : float = x*x
let k x = x*x : float

f 3
g 3.0
h 3.0
k 3.0

printfn "%d %f %f %f" (f 3) (g 3.0) (h 3.1) (k 3.1)
