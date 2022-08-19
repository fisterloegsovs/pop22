module Queue

type 'a queue = Q of 'a list                 // BAD
let empty () = Q []                          // BAD
let insert (Q q: 'a queue) v = Q (v::q)      // BAD
let remove (Q q) : ('a * 'a queue) option =  // BAD
  match List.rev q with                      // BAD
    | [] -> None                             // BAD
    | x::xs -> Some (x,Q (List.rev xs))      // BAD
