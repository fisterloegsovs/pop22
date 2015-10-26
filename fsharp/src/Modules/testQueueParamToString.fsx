let q0 = Queue.empty : Queue.Queue<int>
printfn "q0: %s" (string q0)
let q1 = Queue.put 1 q0
printfn "q1: %s" (string q1)
let q2 = Queue.put 3 ( Queue.put 2 q1 )
printfn "q2: %s" (string q2)
let (w, q3) = Queue.get q2
printfn "(w, q3): (%d, %s)" w (string q3)
