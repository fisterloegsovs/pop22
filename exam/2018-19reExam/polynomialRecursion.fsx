let mulByConst  (a:float) (p:float list)= List.map (fun e -> e*a) p
let mulByX (p:float list) = 0.0 :: p
let rec add (p:float list) (q:float list) =
  match (p, q) with
    [],b -> b
    | a,[] -> a
    | a0::aRest, b0::bRest -> (a0+b0)::(add aRest bRest)
let rec mul (p:float list) (q:float list) =
  match (p, q) with
    [],b -> []
    | a,[] -> []
    | a0::aRest, b -> add (mulByConst a0 b) (mulByX (mul aRest b))

let p = [1.0; 3.2; 0.0; -2.0]
let q = [2.0; 1.3]
printfn "p = %A" p
printfn "q = %A" q
printfn "3.0 p = %A" (mulByConst 3.0 p)
printfn "x p = %A" (mulByX p)
printfn "p + q = %A" (add p q)
printfn "p q = %A" (mul p q)
