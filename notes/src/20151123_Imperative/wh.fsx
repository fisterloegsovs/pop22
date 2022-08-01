let mutable prod = 1
let mutable x = 10
let rec wh () =
  if x > 0 then
    prod <- prod*x
    x <- x - 1
    wh ()
  else
    ();;

wh (); printfn "%d" prod
