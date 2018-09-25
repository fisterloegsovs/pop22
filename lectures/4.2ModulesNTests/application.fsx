let a = 0.0
let b = 1.0
let d = 1e-5
let f x = x * exp(x)
let result = metaFunctions.integrate f a b d
printfn "Int_%g^%g f(x) dx = %g" a b result
