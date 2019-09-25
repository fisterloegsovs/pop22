let a = ref 1.0
let b = ref 2.0
let c = a
printfn "a = %g, b = %g, c = %g" !a !b !c
b := 3.0
c := 4.0
printfn "a = %g, b = %g, c = %g" !a !b !c;;
