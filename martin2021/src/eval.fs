  type expr = Int of int | Add of expr*expr | Mul of expr*expr

  let rec evaluate (e:expr) : int =
    match e with
    | Int c -> c
    | Add (a,b) -> evaluate a + evaluate b
    | Mul (a,b) -> evaluate a * evaluate b
  let x = Add(Mul(Int 3,Int 8),Int 8)
  do printfn "evaluate(x)=%d" (evaluate x)
