let fib n =
  match n with
    0 | 1 ->
      1
    | _ ->
      let mutable prev = (0, 1)
      for i = 2 to n do
        prev <- (snd prev, (fst prev) + (snd prev))
      snd prev

for i = 0 to 45 do
  printfn "fib(%d) = %d" i (fib i)
