module Queue     // GOOD Queue Implementation

type 'a queue = Q of 'a list * 'a list
let empty () = Q ([],[])
let insert (Q (b,f)) v = Q (v::b,f)
let remove (Q (b,f)) : ('a * 'a queue) option =
  match f with
    | x :: xs -> Some (x,Q(b,xs))
    | [] ->
      match List.rev b with
        | [] -> None
        | x :: xs -> Some (x,Q([],xs))
