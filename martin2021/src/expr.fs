// Symbolic expressions of one variable

module Expr

type expr = X
          | Const of float
          | Plus of expr * expr
          | Minus of expr * expr
          | Times of expr * expr
          | Divide of expr * expr
          | Power of expr * float
          | Sin of expr
          | Cos of expr
          | Log of expr
          | Exp of expr

let rec eval e x =
  match e with
    | X -> x
    | Const c -> c
    | Plus (e1, e2) -> eval e1 x + eval e2 x
    | Minus (e1, e2) -> eval e1 x - eval e2 x
    | Times (e1, e2) -> eval e1 x * eval e2 x
    | Divide (e1, e2) -> eval e1 x / eval e2 x
    | Power (e, p) -> (eval e x) ** p
    | Sin e -> sin (eval e x)
    | Cos e -> cos (eval e x)
    | Log e -> log (eval e x)
    | Exp e -> exp (eval e x)

let pretty_bad e =
  let par s = "(" + s + ")"
  let rec pp e : string =
    match e with
      | X -> "x"
      | Const c -> sprintf "%g" c
      | Plus (e1, e2) -> par(pp e1 + "+" + pp e2)
      | Minus (e1, e2) -> par(pp e1 + "-" + pp e2)
      | Times (e1, e2) -> par(pp e1 + "*" + pp e2)
      | Divide (e1, e2) -> par(pp e1 + "/" + pp e2)
      | Power (e, p) -> par(pp e + "^" + sprintf "%g" p)
      | Sin e -> "sin " + pp e
      | Cos e -> "cos " + pp e
      | Log e -> "log " + pp e
      | Exp e -> "exp " + pp e
  in pp e

let p_plus = 3   // precedence
let p_times = 4
let p_unop = 5
let p_max = 7
let par p s = if p < 0 then s else "(" + s + ")"

let pretty e =
  let rec pp e : string * int =
    match e with
      | X -> ("x", p_max)
      | Const c -> (sprintf "%g" c, p_max)
      | Plus (e1, e2) -> pp_binop ("+",p_plus) e1 e2
      | Minus (e1, e2) -> pp_binop ("-",p_plus) e1 e2
      | Times (e1, e2) -> pp_binop ("*",p_times) e1 e2
      | Divide (e1, e2) -> pp_binop ("/",p_times) e1 e2
      | Power (e, p) -> pp_binop ("^",p_unop) e (Const p)
      | Sin e -> pp_unop "sin " e
      | Cos e -> pp_unop "cos " e
      | Log e -> pp_unop "log " e
      | Exp e -> pp_unop "exp " e
  and pp_binop (op,p) e1 e2 =
    let (s1,p1) = pp e1
    let (s2,p2) = pp e2
    in (par (p-p1) s1 + op + par (p-p2) s2, p)
  and pp_unop op e =
    let (s,p) = pp e
    in (op + par (p_unop-p) s, p_unop)
  in fst(pp e)

let toLaTeX e =
  let p_divide = 0
  let rec pp e : string * int =
    match e with
    | X -> ("x", p_max)
    | Const c -> (sprintf "%g" c, p_max)
    | Plus (e1, e2) -> pp_binop ("+",p_plus) e1 e2
    | Minus (e1, e2) -> pp_binop ("-",p_plus) e1 e2
    | Times (e1, e2) -> pp_binop ("\\cdot ",p_times) e1 e2
    | Divide (e1, e2) ->
      let (s1,s2,_) = pp_bin p_divide e1 e2
      in ("\\frac{" + s1 + "}{" + s2 + "}", p_max)
    | Power (e, k) ->
      let (s1,s2,p) = pp_bin p_unop e (Const k)
      in (s1 + "^{" + s2 + "}", p)
    | Sin e -> pp_unop "sin " e
    | Cos e -> pp_unop "cos " e
    | Log e -> pp_unop "log " e
    | Exp e ->
      let (s,a) = pp e
      in ("e^{" + par (p_unop-a) s + "}", p_unop)
  and pp_bin p e1 e2 =
    let (s1,p1) = pp e1
    let (s2,p2) = pp e2
    in (par (p-p1) s1, par (p-p2) s2, p)
  and pp_binop (op,p) e1 e2 =
    let (s1,s2,p) = pp_bin p e1 e2
    in (s1 + op + s2, p)
  and pp_unop op e =
    let (s,p) = pp e
    in ("\\"+op + par (p_unop-p) s, p_unop)
  in fst(pp e)


let math s = "\\[\n" + s + "\n\\]"
let doc e =
  "\\documentclass{article}\usepackage{a4wide}\n\\begin{document}\n" +
  e +
  "\n\\end{document}\n"

let rec ddx e =
  match e with
  | X -> Const 1.0
  | Const c -> Const 0.0
  | Plus (e1, e2) -> Plus(ddx e1, ddx e2)
  | Minus (e1, e2) -> Minus (ddx e1, ddx e2)
  | Times (e1, e2) -> Plus (Times (ddx e1, e2), Times (e1, ddx e2))
  | Divide (e1, e2) ->
      Divide(Minus (Times (ddx e1, e2), Times (e1, ddx e2)),
             Power (e2, 2.0))
  | Power (e, p) -> Times (Times (Const p, Power (e, p-1.0)), ddx e)
  | Sin e -> Times (Cos e, ddx e)
  | Cos e -> Times (Minus (Const 0.0, Sin e), ddx e)
  | Log e -> Times (Divide (Const 1.0, e), ddx e)
  | Exp e -> Times (Exp e, ddx e)

let rec simplify e =
  match e with
  | Plus (e1, Const 0.0) -> simplify e1
  | Plus (Const 0.0, e2) -> simplify e2
  | Plus (Const a, Const b) -> Const (a + b)
  | Plus (e1, e2) -> Plus (simplify e1, simplify e2)
  | Minus (e1, Const 0.0) -> simplify e1
  | Minus (Const a, Const b) -> Const (a - b)
  | Minus (e1, Minus(Const 0.0, e2)) -> Plus (simplify e1, simplify e2)
  | Minus (e1, e2) -> Minus (simplify e1, simplify e2)
  | Times (e1, Const 0.0) -> Const 0.0
  | Times (Const 0.0, e2) -> Const 0.0
  | Times (e1, Const 1.0) -> simplify e1
  | Times (Const 1.0, e2) -> simplify e2
  | Times (Const a, Const b) -> Const (a * b)
  | Times (e1,Plus(e2,e3)) ->
    simplify (Plus(Times(e1,e2),Times(e1,e3)))
  | Times (e1,Minus(e2,e3)) ->
    simplify (Minus(Times(e1,e2),Times(e1,e3)))
  | Times (e1, e2) ->
    if e1 = e2 then simplify (Power (e1,2.0))
    else Times (simplify e1, simplify e2)
  | Divide (Const 0.0, e2) -> Const 0.0
  | Divide (e1, Const 1.0) -> simplify e1
  | Divide (e1, e2) -> Divide (simplify e1, simplify e2)
  | Power (e, 1.0) -> simplify e
  | Power (e, c) -> Power (simplify e,c)
  | Sin e -> Sin (simplify e)
  | Cos e -> Cos (simplify e)
  | Log e -> Log (simplify e)
  | Exp e -> Exp (simplify e)
  | _ -> e

let rec simplifyMax e =
  let se = simplify e
  if se = e then e
  else simplifyMax se

let ee = Plus (Divide (Sin X, Cos X),
               Power (Plus (X, Const 1.0), 2.0))

let eqnarray s e =
  "\\begin{eqnarray}\n" + s + " & = & " + toLaTeX e
  + "\\end{eqnarray}\n"

let diff e =
  let tex_e = eqnarray "f(x)" e
  let dedx = ddx e
  let rec loop e acc =
    let se = simplify e
    in if se = e then List.rev (e::acc)
       else loop se (e::acc)
  let xs = List.map (fun e -> eqnarray "f'(x)" e) (loop dedx [])
  tex_e + String.concat "\n\[\n\Longrightarrow\]\n" xs

do printfn "%s" (doc (diff ee))

//do printfn "%s" (pretty (ddx ee))
