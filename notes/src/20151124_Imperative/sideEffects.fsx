// Demonstration of side effect of mutable records

type store = {mutable value : int}

let x = { value=1 }
let y = { value=2 }
let z = y
let mutable t = y

printfn "Starting state: z is bound to y, variable t set to y"
printfn "x = %d, y = %d, z = %d, t = %d" x.value y.value z.value t.value

printfn "Change y.value to 3"
y.value <- 3;
printfn "x = %d, y = %d, z = %d, t = %d" x.value y.value z.value t.value

printfn "Change t to x"
t <- x;
printfn "x = %d, y = %d, z = %d, t = %d" x.value y.value z.value t.value

printfn "Change x.value to 4"
x.value <- 4;
printfn "x = %d, y = %d, z = %d, t = %d" x.value y.value z.value t.value
