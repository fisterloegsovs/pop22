let rec fib n =
  match n with
    0 -> 0
    | 1 -> 1
    | _ ->
      fib (n - 1) + fib (n - 2)

let mutable i = 0
while i <= 45 do
  printfn "fib(%d) = %d" i (fib i)
  i <- i + 1