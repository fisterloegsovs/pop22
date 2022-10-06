module SafeIntQueue
type element = int
type queue = int list

let emptyQueue : queue  = []

let isEmpty (q : queue) = q = []

let dequeue (q:queue) : element option * queue =
    match q with
        | x::xs -> Some x,xs
        | _ -> None,q

let rec enqueue (e:element) (q:queue) =
    match q with
        | [] -> [e]
        | x::xs -> x :: enqueue e xs
