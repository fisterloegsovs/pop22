open queue

let q0 : queue<int> = create ()
printfn "%A, %A, %A" q0 (isEmpty q0) (dequeue q0)
let q1 = create () |> enqueue 1 |> enqueue 2 |> enqueue 3
printfn "%A, %A" q1 (isEmpty q1)

type element = Value of int | Multiply | Plus | Minus | Divide
let q2 = create () |> enqueue (Value 1) |> enqueue Plus |> enqueue (Value 3)
printfn "%A, %A" q2
let (e,q3) = dequeue q2
printfn "%A, %A" e q3
