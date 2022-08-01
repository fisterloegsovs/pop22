open linalg

let A = [[1.0; 2.0]; [3.0; 4.0]]
printfn "Is A a Matrix? %b" (isMatrix A)
printfn "--\nA has size %A" (size A)
printfn "--\nA:"
echo A
let B = [[1.0; 1.0]; [2.0; 2.0]]
printfn "--\nB:"
echo B
printfn "--\nadd A B:"
add A B |> echo
printfn "--\nmap2 (+) A B:"
map2 (+) A B |> echo

(*
printfn "A + B:"
A + B |> echo
*)

printfn "--\nA +. B:"
A +. B |> echo

printfn "--\nA *. B:"
A *. B |> echo

printfn "--\nA -. A:"
A -. A |> echo

printfn "--\nA -. B:"
A -. B |> echo

printfn "--\nA +. A *. B:"
A +. A *. B |> echo

printfn "--\n(A +. A) *. B:"
(A +. A) *. B |> echo

printfn "--\nA +. (A *. B):"
A +. (A *. B) |> echo

printfn "--\n-A:"
-.A |> echo
