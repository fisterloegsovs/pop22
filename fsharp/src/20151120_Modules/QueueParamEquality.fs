/// A demonstration of a module with a parameterised type and ToString overload from H & R, Functional Programming Using F#.
///
/// How to compile:
/// <code>
/// fsharpc --doc:QueueParam.xml -a QueueParam.fsi QueueParamToString.fs
/// fsharpc --doc:testQueueParamToString.xml -r QueueParamToString.dll testQueueParamToString.fsx
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/10/27

/// The implementation of the Queue type. See interface file for detailed comments on its use.
module Queue
exception EmptyQueue
[<CustomEquality;NoComparison>]
type Queue<'a when 'a : equality> =
  {front: 'a list; rear: 'a list}
  member q.list() = q.front @ ( List.rev q.rear )
  override q.ToString () = string ( q.list() )
  override q1.Equals qobj =
    match qobj with
      | :? Queue<'a> as q2 -> q1.list() = q2.list()
      | _ -> false
  override q.GetHashCode() = hash (q.list());
let empty = {front = []; rear = []}
let put y {front = xs; rear = ys} = {front = xs; rear = y::ys}
let rec get = function
  | {front = x::xs; rear = ys} -> (x, {front = xs; rear = ys})
  | {front = []; rear = []} -> raise EmptyQueue
  | {front = []; rear = ys} -> get {front = List.rev ys; rear = []}

