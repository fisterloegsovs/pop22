// 4.3.1
let rec gdc t n =
  match n with
    0 -> t
    | n -> gdc n (t % n)

// 4.3.2
printfn "%b: gdc 4 4 = 4" (4 = (gdc 4 4))
printfn "%b: gdc 3 7 = 1" (1 = (gdc 3 7))
printfn "%b: gdc 14 21 = 7" (7 = (gdc 14 21))
printfn "%b: gdc 21 14 = 7" (7 = (gdc 21 14))
