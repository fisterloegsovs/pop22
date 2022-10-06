module Queue
type element<'a> = 'a
type queue<'a> = element<'a> list

let emptyQueue : queue<_>  = []

let isEmpty (q : queue<_>) = q = []

let dequeue (q:queue<_>) =
    match q with
        | x::xs -> Some x,xs
        | _ -> None,q

let rec enqueue (e:element<_>) (q:queue<_>) =
    match q with
        | [] -> [e]
        | x::xs -> x :: enqueue e xs
