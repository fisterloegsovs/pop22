let rec powerRealInt a n =
    if (n = 0) then 1.0
    elif (n > 0) then (powerRealInt a (n-1)) * a
    else (powerRealInt a (n+1)) / a

let rec mystisk m n =
    if (m = 0) then n 
    elif (m > n) then (mystisk (m-n) n)
    elif (m < n) then (mystisk m (n-m))
    else m

let rec spaceAdd n =
    if (n <= 0) then ""
    else " " + (spaceAdd (n-1))

let rightAlign n s =
    let stringOut = string n
    spaceAdd(s-(String.length stringOut)) + stringOut

[<EntryPoint>]
let main _ =
    printfn "powerRealInt 0.99 -100 = %f" (powerRealInt 0.99 -100)
    // Resultatet er rigtigt men ikke super præcist
    printfn "Mystisk: %d (it computes gcd of m and n)" (mystisk 21 56)
    printfn "rightAlign 17 4 = '%s'" (rightAlign 17 4)
    printfn "rightAlign -17 2 = '%s'" (rightAlign -17 2)
    0