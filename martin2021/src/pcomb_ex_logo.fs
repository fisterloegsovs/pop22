
open PComb

type cmd =
  | SetColor of string
  | Turn of int       // degrees right (0-360)
  | Move of int       // 1 unit = 1 pixel
  | PenUp
  | PenDown

let parse_col : string p =
  parse_token "r" ||| parse_token "g" |||
  parse_token "b"

let parse_color_cmd : cmd p =
  (parse_token "c" ->> parse_token "(" ->>
   parse_col >>- parse_token ")") >>@ SetColor

let parse_cmd : cmd p =
  ((parse_token "m" ->> parse_int) >>@ Move) |||
  ((parse_token "t" ->> parse_int) >>@ Turn) |||
  (parse_token "u" >>@ (fun _ -> PenUp)) |||
  (parse_token "d" >>@ (fun _ -> PenDown)) |||
  parse_color_cmd

let logo_parser (s:string) : cmd list res =
  let tokens = tokenize "mtudc#() \n" s
  let tokens = elimWS tokens
  run (parse_seq parse_cmd >>- eos) tokens

let logo = "t70um12c(g)dm65"
do printfn "cmds=%A" (logo_parser logo)
