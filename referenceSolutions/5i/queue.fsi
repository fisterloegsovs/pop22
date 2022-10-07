module Queue

type element<'a> = 'a
type queue<'a> = element<'a> list
val emptyQueue : queue<'a>
val enqueue : element<'a> -> queue<'a> -> queue<'a>
val dequeue : queue<'a> -> element<'a> option * queue<'a>
val isEmpty : queue<'a> -> bool when 'a: equality
