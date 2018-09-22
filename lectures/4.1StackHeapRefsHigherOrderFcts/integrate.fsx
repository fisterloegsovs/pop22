/// Estimate the integral of f
/// from a to b with stepsize d
let integrate f a b d =
  let mutable sum = 0.0
  let mutable x = a
  while x < b do
    sum <- sum + d * (f x)
    x <- x + d
  sum

let a = 0.0
let b = 1.0
let d = 0.01
let result = integrate exp a b d
printfn "Int_%g^%g exp(x) dx = %g" a b result
