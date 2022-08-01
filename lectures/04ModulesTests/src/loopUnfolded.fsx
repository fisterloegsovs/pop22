let printDoubles n =
  // Unfolding assuming n = 3
  //  for i = 1 to n do
  //    printf "%d " (2*i)
  (
    let i = 1
    printf "%d " (2*i)
  )
  (
    let i = 2
    printf "%d " (2*i)
  )
  (
    let i = 3
    printf "%d " (2*i)
  )
  printfn ""

printDoubles 3
