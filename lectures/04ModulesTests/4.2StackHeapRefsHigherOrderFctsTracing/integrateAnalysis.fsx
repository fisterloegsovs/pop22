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
let truth = exp 1.0 - 1.0
for e = 0 to 6 do
  let d = 10.0**(float -e)
  let result = truth - integrate exp a b d
  printfn "d = %e: exp 1.0 - 1.0 - Int_%g^%g exp(x) dx = %g" d a b result
