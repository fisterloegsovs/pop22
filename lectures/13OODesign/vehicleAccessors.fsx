type vehicle (w : int) =
  do printfn("Creating a vehicle object");
  let mutable _wheels = w;
  member this.wheels
    with get () = _wheels
    and set (n) = _wheels <- n
    
let car = vehicle 4;
printfn "A car has %d wheels" car.wheels
car.wheels <- 6;
printfn "A modded car has %d wheels" car.wheels

