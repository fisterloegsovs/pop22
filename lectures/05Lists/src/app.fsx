let f x = 2 * x

let g = f 2

open lib

let h = f 2

printfn "%d, %d" g h
