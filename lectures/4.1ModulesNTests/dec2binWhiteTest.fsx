open convert

printfn "White-box testing of dec2bin.fsx"
printfn "  Unit: dec2bin"
printfn "    %5b: Branch 1a" (dec2bin -1 = "Illegal value")
printfn "    %5b: Branch 2a" (dec2bin 0 = "0b0")
printfn "    %5b: Branch 3a" (dec2bin 1 = "0b1")
