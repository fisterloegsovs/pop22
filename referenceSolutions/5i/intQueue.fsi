module IntQueue

type element = int
type queue = int list
val emptyQueue : queue
val enqueue : element -> queue -> queue
val dequeue : queue -> element * queue
val isEmpty : queue -> bool
