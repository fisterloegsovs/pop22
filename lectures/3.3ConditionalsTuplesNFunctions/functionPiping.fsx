let f x = x + 1
let g x = x * x

let a = g (f 2)
let b = 2 |> f |> g
let c = g <| (f <| 2)
do printfn "a = %A, b = %A, c = %A" a b c
