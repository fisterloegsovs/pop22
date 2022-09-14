/// <summary>
/// Given parameters a, b, and c, solve for x
/// when a*x*x + b*x + c = 0
/// </summary>
/// <param name="a">Quadratic coefficient.</param>
/// <param name="b">Linear coefficient.</param>
/// <param name="c">Constant coefficient.</param>
/// <param name="sgn">+1 or -1 determines the solution.</param>
/// <returns>The solution to x.</returns>
let solve (a: float) (b: float) (c: float) : float*float =
  let sqrtd = sqrt (b ** 2.0 - 4.0 * a * c) // We assume that d >= 0
  ((-b - sqrtd) / (2.0 * a), (-b + sqrtd) / (2.0 * a))

// Example use of solve
let a = 1.0
let b = 0.1
let c = -1.0
let x = solve a b c
printfn "%Ax^2+%Ax+%A=0 => x=%A" a b c x