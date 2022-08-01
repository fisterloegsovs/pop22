// Demonstration of a simple counter without references

let mutable tic = 0;

let incr count =
  count + 1

printfn "%d" tic
tic <- incr tic
printfn "%d" tic
tic <- incr tic
printfn "%d" tic
