//module LinAlg
(*
let rec zip = function
  | ([],_)
  | (_,[]) -> []
  | (x::xs,y::ys) -> (x,y)::(zip (xs, ys))

let rec zipzip = function
  | ([],_)
  | (_,[]) -> []
  | (xs::xss,ys::yss) -> (zip (xs,ys))::(zipzip (xss, yss))
*)

let rec tr = function
  | (_::_)::_ as M -> List.map List.head M :: tr (List.map List.tail M)
  | _ -> []

let rec add = function
  | (x::xs,y::ys) -> (x+y)::add (xs,ys)
  | _ -> []

let rec addadd = function
  | (xs::xss,ys::yss) -> (add (xs,ys))::(addadd (xss,yss))
  | _ -> []

let rec dot = function
  | (x::xs,y::ys) -> x*y + (dot (xs,ys))
  | _ -> 0.0

let mul (xss,yss) = List.fold (fun ex xs -> (List.fold (fun ey ys -> (dot (ys,xs))::ey) [] (tr yss))::ex) [] xss

type Vector = V of float list
let length (V(xs)) = List.length xs
type Vector with
  static member ( * )  (a, V(xs)) = V(List.map (fun x -> a*x ) xs)
  static member ( + ) (V(xs),V(ys)) =
    if length (V(xs)) <> length (V(ys)) then
      failwith "Sum of 2 vectors of unequal lengths is undefined"
    else
      V(add (xs,ys))
  static member ( * )  (V(xs), V(ys)) =
    if length (V(xs)) <> length (V(ys)) then
      failwith "Product of 2 vectors of unequal lengths is undefined"
    else
      dot (xs,ys)
  static member ( ~- ) (V(xs)) = (-1.0)*V(xs)
  static member ( - ) (V(xs),V(ys)) = V(xs) + (- V(ys))
let VectorNorm (V(xs)) = sqrt(V(xs) * V(xs))
let VectorToString = function
  | (V(x::xs)) -> "[ " + (string x) + (List.fold (fun e x -> e+", "+(string x)) "" xs) + " ]"
  | V([]) -> "[]"

type Matrix = M of float list list
let rows (M(xss)) = List.length xss
let cols = function
  | M(xs::xss) -> List.length xs
  | M([]) -> 0
let transpose (M(xss)) =
  M(tr xss)
type Matrix with
  static member ( * )  (a, M(xss)) = M(List.map (fun xs -> List.map (fun x -> a*x) xs ) xss)
  static member ( + ) (M(xss),M(yss)) =
    match (xss,yss) with
      | ([],[]) | (_,[]) | ([],_)->  M([])
      | _ ->
        if (rows (M(xss)) <> rows (M(yss))) || (cols (M(xss)) <> cols (M(yss))) then
          failwith "Sum of 2 matrices of unequal sizes is undefined"
        else
          M(addadd (xss,yss))
  static member ( * )  (M(xss), M(yss)) =
    match (xss,yss) with
      | ([],[]) | (_,[]) | ([],_)->  M([])
      | _ ->
        if rows (M(xss)) <> cols (M(yss)) then
          failwith "Product of matrices A in R^{nxm} and B in R^{pxq} is not defined when m <> p"
        else
          M(mul (xss,yss))
  static member ( ~- ) (M(xss)) = (-1.0)*M(xss)
  static member ( - ) (M1,M2) = M1 + (- M2)
let MatrixToString = function
  | (M(xs::xss)) -> "[ " + (VectorToString (V(xs))) + (List.fold (fun e xs -> e+", "+(VectorToString (V(xs)))) "" xss) + " ]"
  | (M([])) -> "[[]]"
  
let v0 = V([1.0..5.0])
printfn "v0 in R^%d =\n%s" (length v0) (VectorToString v0)

let v1 = -v0
printfn "-v0 = v1 in R^%d =\n%s" (length v1) (VectorToString v1)

let v2 = v0+V([0.0..0.1..0.4])
printfn "V0 + V([0.0..0.1..0.4]) = v2 in R^%d =\n%s" (length v2) (VectorToString v2)

let v3 = v0-V([0.0..0.1..0.4])
printfn "v0-V([0.0..0.1..0.4]) = v3 in R^%d =\n%s" (length v3) (VectorToString v3)

let v4 = 3.0*v3
printfn "3.0*v3 = v4 in R^%d =\n%s" (length v4) (VectorToString v4)

let v5 = v3*v4
printfn "v3*v4 = v5 = %f" v5

let v6 = VectorNorm v3
printfn "VectorNorm v3 = v6 = %f" v6

let m0 = M([[1.0; 2.0];[3.0;4.0]])
printfn "m0 in R^(%dx%d) =\n%s" (rows m0) (cols m0) (MatrixToString m0)

let m1 = 3.0*m0
printfn "3.0*m0 = m1 in R^(%dx%d) =\n%s" (rows m1) (cols m1) (MatrixToString m1)

let m2 = -m0
printfn "-m0 = m2 in R^(%dx%d) =\n%s" (rows m2) (cols m2) (MatrixToString m2)

let m3 = m0+M([[5.0; 6.0];[7.0;8.0]])
printfn "m0+M([[5.0; 6.0];[7.0;8.0]]) = m3 in R^(%dx%d) =\n%s" (rows m3) (cols m3) (MatrixToString m3)

let m4 = m0-M([[5.0; 6.0];[7.0;8.0]])
printfn "m0-M([[5.0; 6.0];[7.0;8.0]]) = m4 in R^(%dx%d) =\n%s" (rows m4) (cols m4) (MatrixToString m4)

let m5 = transpose m0
printfn "transpose m0 = m5 in R^(%dx%d) =\n%s" (rows m5) (cols m5) (MatrixToString m5)

let m6 = m0*m5
printfn "m0*m5 = m6 in R^(%dx%d) =\n%s" (rows m6) (cols m6) (MatrixToString m6)

