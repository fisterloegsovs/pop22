// Attempt of makeCounter fails, since return is copy by value, hence it's not mutable outside makeCounter

let incr count =
  count + 1

let makeCounter () =
  let mutable tic = 0;
  tic

let tic = makeCounter ()
printfn "%d" tic
tic <- incr tic
printfn "%d" tic
tic <- incr tic
printfn "%d" tic
