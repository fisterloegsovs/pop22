let mul a =
    let f x = a * x
    f
let f = mul 2
printfn "%A, %A" (mul 3) (f 3)
