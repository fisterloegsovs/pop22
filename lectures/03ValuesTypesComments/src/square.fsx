let discriminant a b c =
  b ** 2.0 - 4.0 * a * c

let solution a b c sgn =
  let d = discriminant a b c
  (-b + sgn * sqrt d) / (2.0 * a)

let a = 1.0
let b = 0.3
let c = -1.0
let xp = (solution a b c +1.0)
printfn "0 = %fx^2 + %fx + %f => x_+ = %f" a b c xp
