let f () =
  let mutable y = 3;
  let z = y;
  let g = fun x -> x*z;
  g
  
let g = f ()
printfn "%d" (g 2)
