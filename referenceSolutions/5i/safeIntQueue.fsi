module SafeIntQueue

type element = int
type queue = int list
val emptyQueue : queue
val enqueue : element -> queue -> queue
val dequeue : queue -> element option * queue
val isEmpty : queue -> bool
