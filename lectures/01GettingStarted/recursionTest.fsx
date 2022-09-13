let rec sum i =
  match i with
    0 -> 0
    | _ -> i + sum (i-1)
let n = 2
printfn "i = %A, sum = %A" n (sum n)