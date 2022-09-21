let ctf str v = str + ", " + (string v)
let lst = [0 .. 2]
let aString = List.fold ctf "" lst
printfn "%A vs. %A" lst aString
