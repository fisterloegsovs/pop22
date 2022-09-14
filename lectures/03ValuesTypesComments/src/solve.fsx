/// Given parameters a, b, and c, solve for x
/// when a x*x + b*x + c =0
let solve (a: float) (b: float) (c: float) : float*float = (-3.4, 2.1)

let a = 1.0
let b = 0.0
let c = -1.0
let x = solve a b c
printfn "%Ax^2+%Ax+%A=0 => x=%A" a b c x