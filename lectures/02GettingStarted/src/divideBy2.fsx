let rec divideByTwo (n: uint) : string =
  match n with
    0u -> ""
    | _ -> (divideByTwo (n/2u)) + (string (n%2u))

let N = 11u
let str = divideByTwo N
printfn "%A_10 = %A_2" N str
