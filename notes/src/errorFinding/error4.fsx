type Animal (animalWeight:float) = class

  member val animalWeight = animalWeight with get, set
  member val neccesaryFoodIntake = 0.0 with get, set
  member val FoodIntake = 0.5
  member this.NeccesaryFoodIntake = 
    this.neccesaryFoodIntake <- this.animalWeight * this.FoodIntake

end

let a = new Animal 3.0
printfn "%A, %A, %A %A" a.animalWeight a.neccesaryFoodIntake a.FoodIntake a.NeccesaryFoodIntake
a.NeccesaryFoodIntake
printfn "%A, %A, %A %A" a.animalWeight a.neccesaryFoodIntake a.FoodIntake a.NeccesaryFoodIntake
a.animalWeight <- 1.0
printfn "%A, %A, %A %A" a.animalWeight a.neccesaryFoodIntake a.FoodIntake a.NeccesaryFoodIntake
a.NeccesaryFoodIntake
printfn "%A, %A, %A %A" a.animalWeight a.neccesaryFoodIntake a.FoodIntake a.NeccesaryFoodIntake
