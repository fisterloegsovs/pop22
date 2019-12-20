let N = 3
let doit n = 
  for i = 1 to n do
    let p = i * i
    printfn "%d: %d" i p

doit N
