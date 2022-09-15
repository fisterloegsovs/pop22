let rec divideByTwo (n: uint) : string =
  match n with
    0u -> ""
    | _ -> (divideByTwo (n/2u)) + (string (n%2u))

let N = 12341u
let str = divideByTwo N
printfn "%d_10 = %s_2" N str
