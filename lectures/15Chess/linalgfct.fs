module linalg
type Scalar = float
type Vector = Scalar list
type Matrix = Vector list
let isMatrix (a: Matrix) : bool =
    match a with
        [] -> true
        | fst::rst -> 
            let col = fst.Length
            List.forall (fun (l: Vector) -> l.Length = col) a
let size (a: Matrix) : int*int =
    (a.Length, a.Head.Length)
let echo (a: Matrix) : unit =
    List.iter (fun l -> List.iter (printf "%A ") l; printfn "") a
let add (a: Matrix) (b: Matrix) : Matrix =
    List.map2 (fun l1 l2 -> List.map2 (fun e1 e2 -> e1+e2) l1 l2) a b
let map2 (op: Scalar->Scalar->Scalar) (a: Matrix) (b: Matrix) : Matrix =
    List.map2 (fun l1 l2 -> List.map2 op l1 l2) a b

// operator overloading: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/operator-overloading
//let inline (+) a b = map2 (+) a b

// New names solves the problem of overshadowing
let inline (+.) a b = map2 (+) a b
let inline ( *. ) a b = map2 (*) a b
let inline ( -. ) a b = map2 (-) a b

// unary minus
let map (f: Scalar->Scalar) (m: Matrix) : Matrix =
    List.map (fun l -> List.map f l) m
let inline (~-.) a = map (~-) a
