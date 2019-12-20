type car (f : float) =
  do printfn("Creating a car object");
  member this.wheels = 4
  member this.fuelEfficiency = f

type bike (w : float) =
  do printfn("Creating a bike object");
  member this.wheels = 2
  member this.weight = w

let car = car 21.5;
let bike = bike 15.2;
printfn "A car has %d wheels and runs %.2f km/l" car.wheels car.fuelEfficiency
printfn "A bike has %d wheels and weighs %.2f kg" bike.wheels bike.weight

