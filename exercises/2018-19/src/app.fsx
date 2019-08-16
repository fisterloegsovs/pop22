open vec2d
let v = (1.0, 2.0)
let w = (3.0, 5.0)
printfn "v = %A" v
printfn "w = %A" w
printfn "len v = %A" (len v)
printfn "ang v = %A" (ang v)
printfn "3.0 * v = %A" (scale 3.0 v)
printfn "add v w = %A" (add v w)
printfn "dot v w = %A" (dot v w)
