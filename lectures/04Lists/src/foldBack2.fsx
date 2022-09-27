let ctb v str = str + ", " + (string v)
let lst = [0 .. 2]
let aString = List.foldBack ctb lst[..lst.Length-2] (string lst[lst.Length-1])
printfn "%A vs. %A" lst aString
