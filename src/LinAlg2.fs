//module LinAlg
type Vector = V of float * (float list)
let VectorMap f (V(x,xs)) = V(f x, List.map f xs)
let VectorFold f e (V(x,xs)) = List.fold f (f e x) xs
let VectorLength = VectorFold (fun e x -> e+1) 0
let listLength = List.fold (fun e x -> e+1) 0
let coord (V(x,xs)) = x::xs
let rec zip = function
  | ([],_) -> []
  | (_,[]) -> []
  | (x::xs,y::ys) -> (x,y)::(zip (xs, ys))
let make  = function
  | (x::xs) -> V(x,xs)
  | _ -> V(0.0,[])
type Vector with
  static member ( ~- ) (V(x,xs)) = VectorMap (fun x -> -x) (V(x,xs))
  static member ( + ) (V(x1,xs1),V(x2,xs2)) =
    if listLength xs1 = listLength xs2 then
      let zipped = (x1,x2)::(zip (xs1,xs2))
      let added = List.map (fun (a,b) -> a+b) zipped
      Some (make added)
    else None
let print (V(x,xs)) = printf "[ %f" x; List.iter (printf ", %f") xs; printfn "]"

let v = make([1.0..5.0])
print v
printfn "v has length: %d" (VectorLength v)

let w = -v
print w
printfn "w has length: %d" (VectorLength w)

let u = Option.get <| v+make([0.0..0.1..0.4])
print u
printfn "u has length: %d" (VectorLength u)
