module Arith

// arithmetic expressions
type expr = 
   Const of int
 | Add of expr * expr
 | Mul of expr * expr
 | Sub of expr * expr
 | Div of expr * expr

// applicative functor/lifting binary functions from int to option int 
let lift2 = Option.map2

// evaluation function for arithmetic expressions
let rec eval (e : expr) : int option =
     match e with 
       Const v -> Some v
     | Add (e1, e2) -> lift2 (+) (eval e1) (eval e2)
     | Mul (e1, e2) -> lift2 (*) (eval e1) (eval e2)
     | Sub (e1, e2) -> lift2 (-) (eval e1) (eval e2)
     | Div (e1, e2) -> match eval e2 with
                           Some 0 -> None
                         | v2 -> lift2 (/) (eval e1) v2