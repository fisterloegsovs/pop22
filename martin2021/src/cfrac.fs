
let eps = 0.00000001
let abs x = if x < 0.0 then -x else x

let float2cfrac (f:float) : int list =
  let rec conv (n:int) (f:float) : int list =
    if n <= 0 then []
    else let q = int(f+eps)
         let r = f - float(q)
         in (printf "f=%A; q=%A; r=%A\n" f q r;
             if n <= 0 || abs r < eps then [q]
             else q::conv (n-1) (1.0/r))
  in conv 10 f

let t1 = float2cfrac 3.245
do printf "%A\n" t1

let t2 = float2cfrac (sqrt 2.0)
do printf "%A\n" t2
