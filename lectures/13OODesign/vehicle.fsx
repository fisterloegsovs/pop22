type vehicle (w : int) =
  do printfn("Creating a vehicle object");
  let _wheels = w;
  member this.wheels = _wheels

let car = vehicle 4;
let bike = vehicle 2;
printfn "A car has %d wheels, a bike has %d wheels" car.wheels bike.wheels

