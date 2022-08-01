let ctf str v = str + ", " + (string v)
let lst = [0 .. 2]
let aString = List.fold ctf (string lst.[0]) lst.[1..]
printfn "%A vs. %A" lst aString
