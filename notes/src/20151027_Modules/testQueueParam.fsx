/// A demonstration of a module with a parameterised type from H & R, Functional Programming Using F#.
///
/// How to compile:
/// <code>
/// fsharpc --doc:QueueParam.xml -a QueueParam.fsi QueueParam.fs
/// fsharpc --doc:testQueueParam.xml -r QueueParam.dll testQueueParam.fsx
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/10/27

/// We make a queue of integers, put a few numbers in it, and get the top.
let q0 = Queue.empty : Queue.Queue<int>
let q1 = Queue.put 1 q0
let q2 = Queue.put 3 ( Queue.put 2 q1 )
let (w1, q3) = Queue.get q2
printfn "%d" w1
let (w2, q4) = Queue.get q3
printfn "%d" w2
let (w3, q5) = Queue.get q4
printfn "%d" w3
