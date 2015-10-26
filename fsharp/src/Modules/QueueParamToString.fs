module Queue
exception EmptyQueue
type Queue<'a> =
  {front: 'a list; rear: 'a list}
  override q.ToString () = string ( q.front @ ( List.rev q.rear ) )
let empty = {front = []; rear = []}
let put y {front = xs; rear = ys} = {front = xs; rear = y::ys}
let rec get = function
  | {front = x::xs; rear = ys} -> (x, {front = xs; rear = ys})
  | {front = []; rear = []} -> raise EmptyQueue
  | {front = []; rear = ys} -> get {front = List.rev ys; rear = []}

