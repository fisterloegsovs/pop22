let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1)

let rec power = function
    | (x,0) -> 1
    | (x,n) -> x * power(x,n-1)

let PI = System.Math.PI

let ringArea inner outer = (outer*outer-inner*inner)*PI

let fooint n k = 2*n-k*k
let fooreal n k = 2.0*n-k*k
let foomix n k = 2.0*n-float(k*k)

let rec factif n =
    if (n = 0) then 1
    else n * factif(n-1)

let rec powerif x n =
    if (n = 0) then 1
    else x * (powerif x (n-1))

// Start main fuction and print results
[<EntryPoint>]
let main _ =
    printfn "fact 0: %i" (fact 0)
    printfn "fact 7: %i" (fact 7)
    printfn "fact 25: %i" (fact 25) // stack overflow
    //printfn "fact -3: %i" (fact -3) // stack overflow
    printfn "power(2,4): %i" (power(2,4))
    //printfn "power(2.0,4): %i" (power(2.0,4)) // no compile - type mismatch
    printfn "ringArea 1.4 4.4: %f" (ringArea 1.4 4.4)
    printfn "fooint 3 2: %i" (fooint 3 2)
    printfn "fooreal 3.0 2.0: %f" (fooreal 3.0 2.0)
    printfn "foomix 3.0 2: %f" (foomix 3.0 2)
    printfn "factif 7: %i" (factif 7)
    printfn "powerif 2 4: %i" (powerif 2 4)
    0