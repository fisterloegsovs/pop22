open cyclicQueue

printfn "Create a queue of 3 elements"
create 3
printfn "Content: %A" (toString ())
printfn "Empty? %A" (isEmpty ())
printfn "Dequeue from an empty queue"
printfn "Value = %A" (dequeue ())
printfn "Content: %A" (toString ())
printfn "Empty? %A" (isEmpty ())
printfn "Add a single element"
printfn "Success? %A" (enqueue 1)
printfn "Content: %A" (toString ())
printfn "Empty? %A" (isEmpty ())
printfn "Dequeue from a singleton queue"
printfn "Value = %A" (dequeue ())
printfn "Content: %A" (toString ())
printfn "Empty? %A" (isEmpty ())
printfn "Fill the queue"
let mutable i = 1
while (enqueue i) do
  i <- i + 1
printfn "Content: %A" (toString ())
printfn "Empty? %A" (isEmpty ())
printfn "Try adding an element to a full queue"
printfn "Success? %A" (enqueue 4)
printfn "Content: %A" (toString ())
printfn "Empty? %A" (isEmpty ())
printfn "Dequeue from a full queue"
printfn "Value = %A" (dequeue ())
printfn "Content: %A" (toString ())
printfn "Empty? %A" (isEmpty ())
printfn "Add an element crossing the boundary"
printfn "Success? %A" (enqueue 5)
printfn "Content: %A" (toString ())
printfn "Empty? %A" (isEmpty ())
printfn "Try adding an element to a full queue"
printfn "Success? %A" (enqueue 4)
printfn "Content: %A" (toString ())
printfn "Empty? %A" (isEmpty ())
printfn "Empty the queue"
while (dequeue ()).IsSome do
  i <- i
printfn "Content: %A" (toString ())
printfn "Empty? %A" (isEmpty ())
