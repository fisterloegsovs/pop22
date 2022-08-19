let transpose (a:'a [,]) : 'a [,] =
  Array2D.init (Array2D.length2 a) (Array2D.length1 a) (fun x y -> a.[y,x])

let a = Array2D.init 5 4 (fun x y -> x*4 + y)
let b = transpose a

do printf "%A\n" a
do printf "%A\n" b

let res = true
do printf "%b\n" res
