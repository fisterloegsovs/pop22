// Unit : dec2bin
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

printfn "White-box testing of dec2bin.fsx"
printfn "  Unit: dec2bin"
printfn "    Branch: 1a - %b" (dec2bin -1 = "Illegal value")
printfn "    Branch: 2a - %b" (dec2bin 0 = "0b0")
printfn "    Branch: 3a - %b" (dec2bin 1 = "0b1")
