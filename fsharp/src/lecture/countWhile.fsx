let countWhile n =
  let mutable i = 0
  while i <= n do
    printf "%d " i
    i <- i + 1; 
  printfn ""

countWhile 3
countWhile 10
countWhile 0
countWhile -1
  
