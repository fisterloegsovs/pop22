type data = {name: string; mutable age: int}
let jon = {name = "Jon"; age = 53}
let mads = {name = "Mads"; age = jon.age}
mads.age <- 28
printfn "Jon's age is %A, and Mads' age is %A" jon.age mads.age
