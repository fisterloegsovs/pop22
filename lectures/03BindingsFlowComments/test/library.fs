module Library

let fib N =
  if N < 3 then
    1
  else
    let mutable pair = (1,1)
    for i = 3 to N do
      pair <- (snd pair, fst pair + snd pair)
    snd pair
