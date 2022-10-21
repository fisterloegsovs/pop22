let rec collatz (n: uint) : uint list =
  n ::
    match n with
      0u | 1u -> []
      | m when m%2u = 0u -> collatz (m/2u)
      | _ -> collatz (3u*n+1u)

let N = 19u
printfn "collats(%d) = %A" N (collatz N)

