let rec sum (acc: uint64) (n: uint64) : uint64 =
  match n with
    m when m < 2UL -> m+acc
    | _ -> sum (n+acc) (n-1UL)

let n = uint64 1e4
printfn "%A" (sum 0UL n)
