module SimpleQueue

// Isomorphism between 'a list and ('a * 'a list) option
let toList v = 
    match v with
        Option.None -> []
      | Option.Some (h, t) -> h :: t 
let fromList (xs : 'a list) : ('a * 'a list) option = 
    match xs with
         [] -> Option.None
       | y :: ys -> Option.Some (y, ys)  

// postcondition: if zs = append' xs ys then zs = xs @ ys
let rec append' xs ys =
    match xs with
        [] -> ys
      | x :: xs' -> x :: append' xs' ys

// (x :: xs') @ ys = x :: (xs' @ ys)
type 'a queue = 'a list 
let empty : 'a queue = []
let enqueue (q: 'a queue, x: 'a) = q @ [x] // ouch
let dequeue (q: 'a queue) = fromList q

let q n = List.fold (fun q v -> enqueue (q, v)) empty [1..n]