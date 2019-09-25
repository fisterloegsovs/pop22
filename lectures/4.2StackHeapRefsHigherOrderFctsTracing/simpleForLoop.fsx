let doit n = 
  for i = 1 to n do
    let p = i * i
    printfn "%d: %d" i p

let N = 3
doit N
