/// A demonstration of a module with a parameterised type and ToString overload from H & R, Functional Programming Using F#.
///
/// How to compile:
/// <code>
/// fsharpc --doc:QueueParam.xml -a QueueParam.fsi QueueParamToString.fs
/// fsharpc --doc:testQueueParam.xml -r QueueParamToString.dll testQueueParam.fsx
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/10/27

/// We make a queue of integers, add a few numbers and use the ToString function to print to screen
let (v,q0) = Queue.get (Queue.put 2 (Queue.put 1 (Queue.empty : Queue.Queue<int>)))
printfn "q0: %s" (string q0)
let q1 = Queue.put 2 (Queue.empty : Queue.Queue<int>)
printfn "q1: %s" (string q1)

printfn "q0 = q1? %b" (q0=q1)
//printfn "q0 > q1? %b" (q0>q1)
