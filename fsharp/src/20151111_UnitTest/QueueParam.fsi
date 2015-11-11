module Queue
/// A demonstration of a module with a parameterised type from H & R, Functional Programming Using F#.
///
/// How to compile:
/// <code>
/// fsharpc --doc:QueueParam.xml -a QueueParam.fsi QueueParam.fs
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/10/27

/// A Queue of parameterised type.
type Queue<'a>
/// make an empty queue
val empty : Queue<'a>
/// Add an element to the back of a queue
val put : 'a -> Queue<'a> -> Queue<'a>
/// remove the front element of a queue
val get : Queue<'a> -> 'a * Queue<'a>
/// get may raise exception on empty list
exception EmptyQueue
