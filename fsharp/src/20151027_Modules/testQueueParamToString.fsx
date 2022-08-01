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
let q0 = Queue.empty : Queue.Queue<int>
printfn "q0: %s" (string q0)
let q1 = Queue.put 1 q0
printfn "q1: %s" (string q1)
let q2 = Queue.put 3 ( Queue.put 2 q1 )
printfn "q2: %s" (string q2)
let (w, q3) = Queue.get q2
printfn "(w, q3): (%d, %s)" w (string q3)

/// We now make a queue of chars and print it using the same library
let p = Queue.put 'c' (Queue.put 'b' (Queue.put 'a' (Queue.empty : Queue.Queue<char>)))
printfn "p: %s" (string p)


