module queue

type queue<'a> = 'a list // a queue of elements

let create (): queue<'a> = []
let enqueue (e: 'a) (q: queue<'a>) : queue<'a> = q@[e]
let dequeue (q: queue<'a>) : (('a option) * queue<'a>) =
  match q with
    [] -> (None, [])
    | e::rst -> (Some e, rst)
let isEmpty (q: queue<'a>) : bool = q.IsEmpty
