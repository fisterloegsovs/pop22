module IntQueue
type element = int
type queue = int list

let emptyQueue : queue  = []

let isEmpty (q : queue) = q = []

let dequeue (q:queue) =
    match q with
        | x::xs -> x,xs
        | _ -> failwith "oh heavens no, we dequeued an empty queue!"

let rec enqueue (e:element) (q:queue) =
    match q with
        | [] -> [e]
        | x::xs -> x :: enqueue e xs
