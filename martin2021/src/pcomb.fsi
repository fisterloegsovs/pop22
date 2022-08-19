module PComb

type 'a res = Ok of 'a | No of string
type token = string
type 'a p = token list -> ('a * token list) res

val (>*>)       : 'a p -> 'b p -> ('a*'b)p
val (>>@)       : 'a p -> ('a -> 'b) -> 'b p
val (->>)       : 'a p -> 'b p -> 'b p
val (>>-)       : 'a p -> 'b p -> 'a p
val (|||)       : 'a p -> 'a p -> 'a p
val (>>?)       : 'a p -> 'b p -> ('a->'b->'a) -> 'a p

val eos         : unit p
val run         : 'a p -> token list -> 'a res
val tokenize    : string -> string -> token list
val elimWS      : token list -> token list
val parse_int   : int p
val parse_token : token -> token p
val parse_seq   : 'a p -> 'a list p
