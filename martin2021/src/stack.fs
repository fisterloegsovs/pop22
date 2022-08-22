module Stack

type 'a stack = S of 'a list
let empty () = S []
let push  (S s: 'a stack) v = S (v::s)
let pop   (S s) : ('a * 'a stack) option =
  match s with
    | [] -> None
    | x::xs -> Some (x,S xs)
