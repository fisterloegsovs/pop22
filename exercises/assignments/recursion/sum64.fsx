let rec sum (n: uint64) : uint64 =
  match n with
    0UL -> 0UL
    | _ -> n+sum (n-1UL)

let n = uint64 1e4
printfn "%A" (sum n)