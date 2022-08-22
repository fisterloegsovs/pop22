open PComb

type exp = Int of int
         | X
         | Plus of exp * exp
         | Minus of exp * exp
         | Sin of exp

let parse_par (p:'a p) : 'a p =
  parse_token "(" ->> p >>- parse_token ")"

let rec parse_exp : exp p = fun x ->
  ((parse_int >>@ Int) |||
   (parse_token "x" >>@ (fun _ -> X)) |||
   ((parse_par (parse_bin "+" Plus))) |||
   ((parse_par (parse_bin "-" Minus))) |||
   ((parse_token "sin" ->> parse_exp) >>@ Sin) |||
   ((parse_par parse_exp))) x

and parse_bin op f =
  (parse_exp >>- parse_token op >*> parse_exp) >>@ f

let exp_parser s : exp res =
  let tokens = tokenize "() \n+-" s
  let tokens = elimWS tokens
  do printfn "tokens=%A" tokens
  run (parse_exp >>- eos) tokens

let s = "(2-sin(3+x))"

do printfn "e=%A" (exp_parser s)

let test1 = tokenize "+ ()" s = ["34"; "+"; "2"; "+"; "("; "56-23"; ")"; "+"; "24"]
