module queue

/// <summary>Add an element to the end of a queue</summary>
/// <param name="e">an element</param>
val enqueue: e: int -> unit

/// <summary>Remove the element in the front position of the queue</summary>
/// <returns>The first element in q</returns>
val dequeue: unit -> int option

/// <summary>Check if the queue is empty</summary>
/// <returns>True if the que is empty</returns>
val isEmpty: unit -> bool

/// <summary>The queue on string form</summary>
/// <returns>A string representing the queue's elements</returns>
val toString: unit -> string