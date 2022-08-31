let n = 3UL
let m = 5UL
let p = 1000UL
let multiplesOfn = List.map (fun i -> n*i) [1UL..p/n]
let multiplesOfm = List.map (fun i -> m*i) [1UL..p/m]
let sum = List.collect (fun i -> List.map (fun j -> i*j) multiplesOfm) multiplesOfn
          |> List.distinct
          |> List.sum
printfn "The sum of all the multiples of %A or %A below %A is %A" n m p sum
