type vehicle (w : int) =
  do printfn("Creating a vehicle object");
  member this.wheels = w

type car (f : float) =
  inherit vehicle(4)
  do printfn("Creating a car object");
  member this.fuelEfficiency = f

type bike (w : float) =
  inherit vehicle(2)
  do printfn("Creating a bike object");
  member this.weight = w

let aVehicle = vehicle 3
let car = car 21.5
let bike = bike 15.2
printfn "A vehicle has %d wheels" aVehicle.wheels
printfn "A car has %d wheels and runs %.2f km/l" car.wheels car.fuelEfficiency
printfn "A bike has %d wheels and weighs %.2f kg" bike.wheels bike.weight

