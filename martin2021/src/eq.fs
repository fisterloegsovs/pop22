//
// > 3 = 2;;                          > 4.2 = 2.3;;
// val it : bool = false              val it : bool = false
//  > (2,3.2) = (2,3.2);;              > [2;3] = [2;3]
//  val it : bool = true               val it : bool = true
//  > [|2;3|] <> [|2;3|]               > [2;3] = [2;4]
//  val it : bool = false              val it : bool = false


type 'a tree = Leaf of 'a | Tree of 'a tree * 'a tree

type expr = Const of int            // Expression trees
          | Add of expr * expr
          | Mul of expr * expr

let rec eval (e:expr) : int =
  match e with
  | Const c -> c
  | Add (a,b) -> eval a + eval b
  | Mul (a,b) -> eval a * eval b

let a = Add(Mul(Const 3,Const 8),Const 8)  // --> 24 * 8
let b = Add(Const 8,Mul(Const 6,Const 4))  // --> 8 * 24

do printfn "(a=b) = %A" (a=b)
do printfn "(eval a = eval b) = %A" (eval a = eval b)
