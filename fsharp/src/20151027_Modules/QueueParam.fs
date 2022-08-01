module Queue
/// A demonstration of a module with a parameterised type from H & R, Functional Programming Using F#.
///
/// How to compile:
/// <code>
/// fsharpc --doc:QueueParam.xml -a QueueParam.fsi QueueParam.fs
/// fsharpc --doc:testQueueParamxml -r QueueParam.dll testQueueParam.fsx
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/10/27

/// The implementation of the Queue type. See interface file for detailed comments on its use.
exception EmptyQueue
type Queue<'a> = {front: 'a list; rear: 'a list}
let empty = {front = []; rear = []}
let put y {front = xs; rear = ys} = {front = xs; rear = y::ys}
let rec get = function
  | {front = x::xs; rear = ys} -> (x, {front = xs; rear = ys})
  | {front = []; rear = []} -> raise EmptyQueue
  | {front = []; rear = ys} -> get {front = List.rev ys; rear = []}

