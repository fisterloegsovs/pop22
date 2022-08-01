let rec fac n =
  match n with
    n when n > 1 -> n * fac (n-1)
    | _ -> 1

let n = 5
printfn "fac %d = %d" n (fac n)
