let recMulTable n =
  let rec row x y =
    if x > 0 then
      row (x-1) y + sprintf "%3s " (string (x * y))
    else
      ""

  let rec table n =
    if n > 0 then
      sprintf "%s\n%2d%s" (table (n-1)) n (row 10 n)
    else
      ""
      
  printfn "  %s%s" (row 10 1) (table n)

printfn "Multiplication table up to 2:"
recMulTable 2
printfn "Multiplication table up to 10:"
recMulTable 10
