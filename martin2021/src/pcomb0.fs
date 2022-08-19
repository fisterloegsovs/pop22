
// module PComb

type 'a res = Ok of 'a | No of string

type token = string

type 'a p = token list -> ('a * token list) res

let (>*>) (p1:'a p) (p2:'b p) : ('a*'b)p = fun ts ->
  match p1 ts with
    | No s -> No s
    | Ok (v1,ts) ->
      match p2 ts with
        | No s -> No s
        | Ok (v2,ts) -> Ok ((v1,v2),ts)

let (>>@) (p:'a p) (f:'a -> 'b) : 'b p = fun ts ->
  match p ts with
    | No s -> No s
    | Ok (v,ts) -> Ok (f v,ts)

let (->>) p1 p2 = (p1 >*> p2) >>@ (fun (_,y) -> y)
let (>>-) p1 p2 = (p1 >*> p2) >>@ (fun (x,_) -> x)

let (|||) (p1:'a p) (p2:'a p) : 'a p = fun ts ->
  match p1 ts with
    | Ok(v,ts) -> Ok(v,ts)
    | No s1 ->
      match p2 ts with
        | Ok(v,ts) -> Ok(v,ts)
        | No s2 -> No (s1 + " or " + s2)

let (>>?) (p1:'a p) (p2:'b p) : ('a->'b->'a) -> 'a p =
  fun f ts ->
    match p1 ts with
      | No s1 -> No s1
      | Ok(v1,ts) ->
        match p2 ts with
          | Ok(v2,ts) -> Ok(f v1 v2,ts)
          | No s2 -> Ok(v1,ts)

let eos : unit p =
  function [] -> Ok((),[])
         | _ -> No "tokens"

let run (p:'a p) (ts:string list) : 'a res =
  match p ts with
    | Ok (v,_) -> Ok v
    | No s -> No s

// [tokenize cs s] splits s into a list of tokens; splitting occurs at
// boundaries to the characters in cs.

let tokenize (cs:string) (s:string) : string list =
  let extract i n ts =
    if n > 0 then s.Substring (i,n) :: ts
    else ts
  let rec loop i n ts =
    if i+n >= String.length s then
      List.rev(extract i n ts)
    else if String.exists (fun c -> c = s.[i+n]) cs then
      loop (i+n+1) 0 (extract (i+n) 1 (extract i n ts))
    else loop i (n+1) ts
  in loop 0 0 []

// Eliminate white space
let elimWS : token list -> token list =
  List.filter (fun s -> s <> "\n" && s <> " ")

type exp = Int of int
         | X
         | Plus of exp * exp
         | Minus of exp * exp
         | Sin of exp

let parse_int : int p =
  function i::ts -> (try Ok(int(i),ts) with
                       _ -> No "integer")
         | [] -> No "integer"

let parse_token (x:string) : string p =
  function s::ts -> if s = x then Ok(s,ts) else No x
         | [] -> No x

let rec parse_exp : exp p =
  fun x ->
    ((parse_int >>@ Int) |||
     (parse_token "x" >>@ (fun _ -> X)) |||
     ((parse_token "(" ->> parse_bin "+" Plus) >>- (parse_token ")")) |||
     ((parse_token "(" ->> parse_bin "-" Minus) >>- (parse_token ")")) |||
     ((parse_token "sin" ->> parse_exp) >>@ Sin) |||
     ((parse_token "(" ->> parse_exp) >>- (parse_token ")"))) x

and parse_bin op f =
  ((parse_exp >>- (parse_token op)) >*> parse_exp) >>@ f

let exp_parser s : exp res =
  let tokens = tokenize "() \n+-" s
  let tokens = elimWS tokens
  do printfn "tokens=%A" tokens
  run (parse_exp >>- eos) tokens

let s = "(2-sin(3+x))"

do printfn "e=%A" (exp_parser s)

let test1 = tokenize "+ ()" s = ["34"; "+"; "2"; "+"; "("; "56-23"; ")"; "+"; "24"]

type cmd = Mv of int | Rot of int | Up | Down | Color of string

type cmds = cmd list

type colorname = string
let is_color (s:string) : bool =
  List.exists (fun x -> s=x) ["black";"red";"green";"blue";"yellow"]

let parse_color : colorname p =
  fun ts ->
    match ts with
      | t :: ts -> if is_color t then Ok (t,ts) else No "color"
      | [] -> No "tokens"

let parse_color_cmd : cmd p =
  (parse_token "c" ->> parse_token "(" ->> parse_color >>- parse_token ")") >>@ Color

let rec parse_seq (p:'a p) : 'a list p =
  fun x ->
    (((p >>@ (fun e -> [e])) >>? parse_seq p) (fun x y -> x@y)) x

let parse_cmd : cmd p =
  ((parse_token "m" ->> parse_int) >>@ Mv) |||
  ((parse_token "r" ->> parse_int) >>@ Rot) |||
  (parse_token "u" >>@ (fun _ -> Up)) |||
  (parse_token "d" >>@ (fun _ -> Down)) |||
  parse_color_cmd

// let logo = "m5r70uc(yellow)m10r180m10"
let logo = "m5r70uc(yellow)"

let logo_parser (s:string) : cmds res =
  let tokens = tokenize "mrudc() \n" s
  let tokens = elimWS tokens
  do printfn "tokens=%A" tokens
  run (parse_seq parse_cmd >>- eos) tokens

do printfn "cmds=%A" (logo_parser logo)
