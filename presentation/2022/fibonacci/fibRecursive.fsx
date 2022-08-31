let rec fib n =
  match n with
    0 | 1 ->
      1
    | _ ->
      fib (n - 1) + fib (n - 2)

for i = 0 to 45 do
  printfn "fib(%d) = %d" i (fib i)
