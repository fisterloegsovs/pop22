let intQueueTests () =
    let q0 = IntQueue.emptyQueue
    let emptyTestResult = IntQueue.isEmpty q0
    emptyTestResult
    |> printfn "An empty queue is empty: %A"

    let e1,e2,e3 = 1,2,3
    let q1 = q0 |> IntQueue.enqueue e1
                |> IntQueue.enqueue e2
                |> IntQueue.enqueue e3
    let nonEmptyTestResult = not (IntQueue.isEmpty q1)

    nonEmptyTestResult
    |> printfn "A queue with elements is not empty: %A"

    let (e,q2) = IntQueue.dequeue q1
    let dequeueTestResult = e = e1
    dequeueTestResult
    |> printfn "First in is first out: %A" 

    let allTestResults =
        emptyTestResult &&
        nonEmptyTestResult &&
        dequeueTestResult

    allTestResults
    |> printfn "All IntQueue tests passed: %A"
    // Return the test results as a boolean
    allTestResults

let safeIntQueueTests () = 
    let q0 = SafeIntQueue.emptyQueue

    let ieq = SafeIntQueue.enqueue
    let idq = SafeIntQueue.dequeue

    let q1 = q0 |> ieq 1 |> ieq 2 |> ieq 3
    printfn "%A, %A" q1 (SafeIntQueue.isEmpty q1)
    let (e,q2) = idq q1
    let (e',q3) = idq q0
    printfn "%A, %A" e q2
    printfn "%A, %A" e' q3    


let queueTests1 () =
    let eq = Queue.enqueue
    let dq = Queue.dequeue
    let ie = Queue.isEmpty
    // Generic int queues
    let q0 = Queue.emptyQueue<int>
    let q1 = q0 |> eq 2 |> eq 3
    let e,q2 = dq q1
    printfn "%A, %A" e q2
    let e',q3 = dq q0
    printfn "%A, %A" e' q3

let queueTests2 () =
    let eq = Queue.enqueue
    let dq = Queue.dequeue
    let ie = Queue.isEmpty
    // Generic float queues
    let q0 = Queue.emptyQueue<float>
    let q1 = q0 |> eq 2. |> eq 3.
    let e,q2 = dq q1
    printfn "%A, %A" e q2
    let e',q3 = dq q0
    printfn "%A, %A" e' q3


let queueTests3 () =
    let eq = Queue.enqueue
    let dq = Queue.dequeue
    let ie = Queue.isEmpty
    // Generic string queues
    let q0 = Queue.emptyQueue<string>
    let q1 = q0 |> eq "heyo" |> eq "my sweet queue"
    let e,q2 = dq q1
    printfn "%A, %A" e q2
    let e',q3 = dq q0
    printfn "%A, %A" e' q3
    
    
[<EntryPoint>]
let main args =
    intQueueTests () |> ignore
    safeIntQueueTests ()
    // generic int queues
    queueTests1 ()
    // generic float queues
    queueTests2 ()
    // generic string queues
    queueTests3 ()
    0
