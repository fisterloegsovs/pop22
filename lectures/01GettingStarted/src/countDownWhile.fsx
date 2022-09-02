let countDown n =
  let mutable i = n
  while i >= 0 do
    printfn "%A" i
    i <- i - 1

countDown 5
