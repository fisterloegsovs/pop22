let rnd = System.Random()

type Animal (name:string, animalWeight:float, maxSpeed:float) = class

  let mutable foodInTakePercentage = float(rnd.Next(0,101))

  member val animalMaxSpeed : float = maxSpeed with get, set
  member val animalWeight = animalWeight with get, set
  member val neccesaryFoodIntake = 0.0 with get, set
  member val Name = name
  new (name, maxSpeed) =
    let minWeight = 70.0
    let maxWeight = 300.0
    let Weight = minWeight + rnd.NextDouble() * (maxWeight-minWeight)
    Animal (name, Weight, maxSpeed)
  member this.FoodInTakePercentage = foodInTakePercentage/100.0
  member this.CurrentSpeed =
    this.FoodInTakePercentage*maxSpeed
  abstract FoodIntake : float 
  default this.FoodIntake = 0.5 
  member this.NeccesaryFoodIntake = 
    neccesaryFoodIntake <- animalWeight * FoodIntake

end

type Carnivore (name:string, animalWeight:float, maxSpeed:float) = class
  inherit Animal (name, animalWeight, maxSpeed)
  override this.FoodIntake = 0.08
end

type Herbivore (name:string, animalWeight:float, maxSpeed:float) = class
  inherit Animal (name, animalWeight, maxSpeed)
  override this.FoodIntake = 0.4
end
