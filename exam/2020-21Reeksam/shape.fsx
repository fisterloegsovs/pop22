type shape = Circle of r : float
let pi = 3.141592
let radius (s : shape) : float =
  match s with
    Circle r -> r
//
let lst = [Circle 3.0; Circle 1.2; Circle 10.9]
printfn "Circle no 1 has radius: %f" (radius lst.[0])
