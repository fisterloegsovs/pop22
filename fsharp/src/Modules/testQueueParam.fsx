let q0 = Queue.empty : Queue.Queue<int>
let q1 = Queue.put 1 q0
let q2 = Queue.put 3 ( Queue.put 2 q1 )
let (w, q3) = Queue.get q2