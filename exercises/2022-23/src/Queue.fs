module Queue

type 'a queue = 'a list * 'a list
let empty : 'a queue = ([], [])
let enqueue ((frontq, endq) : 'a queue, x: 'a) = (frontq, x :: endq) 
let dequeue ((frontq, endq) : 'a queue) =
    match frontq with
        [] -> match List.rev endq with
                  [] -> None
                | y :: ys -> Some (y, (ys, []))
      | x :: xs -> Some (x, (xs, endq))
                    
let q n = List.fold (fun q v -> enqueue (q, v)) empty [1..n]
   