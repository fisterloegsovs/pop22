//module LinAlg
type Vector = V of float list
let rec zip = function
  | ([],_)
  | (_,[]) -> []
  | (x::xs,y::ys) -> (x,y)::(zip (xs, ys))
let VectorLength (V(xs)) = List.length xs
type Vector with
  static member ( ~- ) (V(xs)) = V(List.map (fun x -> -x) (xs))
  static member ( + ) (V(xs),V(ys)) =
    if VectorLength (V(xs)) <> VectorLength (V(ys)) then
      failwith "Sum of 2 vectors of unequal lengths is undefined"
    else
      let zipped = zip (xs, ys)
      let added = List.map (fun (a,b) -> a+b) zipped
      V(added)
  static member ( - ) (V1,V2) = V1 + - V2
  static member ( * )  (a, V(xs)) = V(List.map (fun x -> a*x ) xs)
  static member ( * )  (V(xs), V(ys)) =
    if VectorLength (V(xs)) <> VectorLength (V(ys)) then
      failwith "Product of 2 vectors of unequal lengths is undefined"
    else
      let zipped = zip (xs, ys)
      List.fold (fun sum (a,b) -> sum + a*b) 0.0 zipped
let norm (V(xs)) = sqrt(V(xs) * V(xs))
let rec VectorPrint = function
  | (V(x::xs)) -> printf "[ %f" x; List.iter (printf ", %f") xs; printfn " ]"
  | V([]) -> printfn "[]"

let v0 = V([1.0..5.0])
printfn "v0 in R^%d = " (VectorLength v0)
VectorPrint v0

let v1 = -v0
printfn "-v0 = v1 in R^%d = " (VectorLength v1)
VectorPrint v1

let v2 = v0+V([0.0..0.1..0.4])
printfn "V0 + V([0.0..0.1..0.4]) = v2 in R^%d = " (VectorLength v2)
VectorPrint v2

let v3 = v0-V([0.0..0.1..0.4])
printfn "v0-V([0.0..0.1..0.4]) = v3 in R^%d = " (VectorLength v3)
VectorPrint v3

let v4 = 3.0*v3
printfn "3.0*v3 = v4 in R^%d = " (VectorLength v4)
VectorPrint v4

let v5 = v3*v4
printfn "v3*v4 = v5 = %f" v5

let v6 = norm v3
printfn "norm v3 = v6 = %f" v6
