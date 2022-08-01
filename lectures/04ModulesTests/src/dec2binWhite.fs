module convert

let dec2bin n =
  if n < 0 then           (* WB: 1 *)
    "Illegal value"
  elif n = 0 then         (* WB: 2 *)
    "0b0"
  else
    let mutable v = n
    let mutable str = ""
    while v > 0 do        (* WB: 3 *)
      str <- (string (v % 2)) + str
      v <- v / 2
    "0b" + str
