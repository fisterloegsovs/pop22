let lst = [0..5]
let sqr x = x * x
let lst2 = List.map sqr lst
printfn "All elements in a %A squared is %A" lst lst2
