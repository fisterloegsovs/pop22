// Attempt of incr on a mutable fails, since return is copy by value, hence it's not inside incr

let mutable tic = 0;

let incr count =
  count <- count + 1
  count

printfn "%d" tic
printfn "%d" (incr tic)
printfn "%d" (incr tic)
