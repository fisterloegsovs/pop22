
let (+.) (a,b) (c,d) = (a+c,b+d): float*float

let x = (1.0, -2.0)
let y = (3.0, 4.0)
let z = x +. y
let (z1, z2) = z

[<EntryPoint>]
let F _ =
    printfn "%f" z1
    0