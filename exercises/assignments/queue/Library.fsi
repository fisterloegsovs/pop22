module queue

type queue<'a> = 'a list // a queue of elements

/// <summary>Create an empty queue</summary>
/// <returns>an empty queue</returns>
val create: unit -> queue<'a> 

// 
/// <summary>Add an element to the end of a queue</summary>
/// <param name="e">an element</param>
/// <param name="q">a queue</param>
/// <returns>an updated queue</returns>
val enqueue: e:'a -> q:queue<'a> -> queue<'a>

// 
/// <summary>Remove the element in the front position of the queue</summary>
/// <param name="q">a queue</param>
/// <returns>The first element in q and the remaining queue</returns>
val dequeue: q:queue<'a> -> ('a option)*queue<'a>

/// <summary>Check if the queue is empty</summary>
/// <returns>True if the que is empty</returns>
val isEmpty: queue<'a> -> bool
