/// A demonstration of a module with a parameterised type and ToString overload from H & R, Functional Programming Using F#.
///
/// How to compile:
/// <code>
/// fsharpc --doc:QueueParam.xml -a QueueParam.fsi QueueParamToString.fs
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/11/11

/// The implementation of the Queue type. See interface file for detailed comments on its use.
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
  | {front = []; rear = ys} -> get {front = ys; rear = []} // forgot to reverse

