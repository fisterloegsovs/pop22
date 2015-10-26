module Queue
type Queue<'a>
val empty : Queue<'a> // make an empty queue
val put : 'a -> Queue<'a> -> Queue<'a> // Add an element to the back of a queue
val get : Queue<'a> -> 'a * Queue<'a> // remove the front element of a queue
exception EmptyQueue // Raised on get on empty list
