/// Given parameters a, b, and c, solve for x
/// when a*x*x + b*x + c = 0
let solve (a: float) (b: float) (c: float) : float*float =
  let sqrtd = sqrt (b ** 2.0 - 4.0 * a * c)
  ((-b - sqrtd) / (2.0 * a), (-b + sqrtd) / (2.0 * a))

let a = 1.0
let b = 0.1
let c = -1.0
let x = solve a b c
printfn "%Ax^2+%Ax+%A=0 => x=%A" a b c x