let ctf str v = str + ", " + (string v)
let lst = [0 .. 2]
let aString = List.fold ctf (string lst.Head) lst.Tail
printfn "%A vs. %A" lst aString
