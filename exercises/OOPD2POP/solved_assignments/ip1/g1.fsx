
// Define descriminate float * float * float -> float
let d a b c = System.Math.Sqrt(b*b-4.0*a*c)

// Define the solution float * float * float -> float * float
let solve2 a b c = 
    (-b-(d a b c))/(2.0*a),
    (-b+(d a b c))/(2.0*a)  

// Compute roots of x^2+2x-5
let (root1,root2) = solve2 2.0 3.0 1.0

let printRoots t =
    let (x, y) = t
    printf "Roots are: %f" x
    printfn " %f" y

let rec powerNew x n = 
    if (n = 0) then 1
    elif (n = 2) then x*x
    elif (n%2 = 0) then powerNew (powerNew x (n/2)) 2
    else x * (powerNew x (n-1))

let rec powerC x n c = 
    // Power function with operation counting
    if (n = 0) then 1, c
    elif (n = 2) then x*x, c+1
    elif (n%2 = 0) then
        let (nx, nc) = (powerC x (n/2) c)
        let (nnx, nnc) = (powerC nx 2 nc)
        nnx, nnc // Add 2 to nnc to count div and mod as well
    else
        let (nx, nc) = (powerC x (n-1) c)
        x * nx, nc+1

let powerCount x n = powerC x n 0

let (x, count) = (powerCount 2 21)

// Start main fuction and print results
[<EntryPoint>]
let main _ =
    printRoots (solve2 2.0 3.0 1.0)
    // Roots are -1.0, -0.5
    printRoots (solve2 2.0 3.0 4.0)
    // Roots NaN NaN (no real roots, NaN occurs due to sqrt of negative number)

    printfn "pwer(2,21) = %d" (powerNew 2 21)

    printfn "Optimized Power function. power(2,21) = %d with %d multiplications" x count
    //
    0