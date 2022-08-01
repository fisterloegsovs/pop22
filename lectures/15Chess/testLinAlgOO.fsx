open linalg

let A = Matrix([[1.0; 2.0]; [3.0; 4.0]])
printfn "Is A a Matrix? %b" (A.isMatrix)
printfn "--\nA has size %A" (A.size)
printfn "--\nA:"
printfn "%A" A
let B = Matrix([[1.0; 1.0]; [2.0; 2.0]])
printfn "--\nB:"
printfn "%A" B
printfn "--\nmap2 (+) A B:"
Matrix.map2 (+) A B |> printfn "%A"

printfn "A + B:"
A + B |> printfn "%A"

printfn "--\nA * B:"
A * B |> printfn "%A"

printfn "--\nA - A:"
A - A |> printfn "%A"

printfn "--\nA - B:"
A - B |> printfn "%A"

printfn "--\nA + A * B:"
A + A * B |> printfn "%A"

printfn "--\n(A + A) * B:"
(A + A) * B |> printfn "%A"

printfn "--\nA + (A * B):"
A + (A * B) |> printfn "%A"

printfn "--\n-A:"
-A |> printfn "%A"

printfn "--\n 3.0+A:"
3.0+A |> printfn "%A"

printfn "--\n 3.0+A:"
A+3.0 |> printfn "%A"
