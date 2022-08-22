exception MyExnArg of int
let f () = if false then 8 else raise (MyExnArg 5)
let y = try f () with | MyExnArg x -> x
do printfn "%d" y
