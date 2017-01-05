type Animal (animalWeight:float) = class

  member val animalWeight = animalWeight with get, set
  member val neccesaryFoodIntake = 0.0 with get, set
  member val FoodIntake = 0.5
  member this.NeccesaryFoodIntake () = 
    this.neccesaryFoodIntake <- this.animalWeight * this.FoodIntake
    this.neccesaryFoodIntake

end

let a = new Animal 3.0
printfn "%A %A" (a.NeccesaryFoodIntake ()) a.neccesaryFoodIntake
a.NeccesaryFoodIntake ()
printfn "%A %A" (a.NeccesaryFoodIntake ()) a.neccesaryFoodIntake
