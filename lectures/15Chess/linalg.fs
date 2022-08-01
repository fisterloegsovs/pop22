module linalg
type Scalar = int
let zero : Scalar = 0
let one : Scalar = 1
type Vector = Scalar list
type Matrix = Vector list
let isMatrix (a: Matrix) : bool =
    match a with
        [] -> true
        | fst::rst -> 
            let col = fst.Length
            List.forall (fun (l: Vector) -> l.Length = col) a
let eye (n: int) : Matrix=
    List.init<Vector> n (fun i -> List.replicate (i-0) zero @ [one] @ List.replicate (n-i-1) zero)
let empty (m:int) : Matrix =
    List.replicate m List.empty<Scalar>
let size (a: Matrix) : int*int =
    (a.Length, a.Head.Length)
let fold (f: 'a -> Scalar -> 'a) (n: 'a) (a: Matrix) : 'a = 
    List.fold (fun (acc: 'a) (e: Vector) -> List.fold f acc e) n a
let foldBack (f: Scalar -> 'a -> 'a) (a: Matrix) (n: 'a) : 'a = 
    List.foldBack (fun (e: Vector) (acc: 'a) -> List.foldBack f e acc) a n
let echo (a: Matrix) : unit =
    List.iter (fun l -> List.iter (printf "%A ") l; printfn "") a
let forall (f: Scalar->bool) (a: Matrix) : bool =
    List.forall (fun l -> List.forall f l) a
let forall2 (f: Scalar->Scalar->bool) (a: Matrix) (b: Matrix) : bool =
    List.forall2 (fun l1 l2 -> List.forall2 f l1 l2) a b
let map (f: Scalar->Scalar) (m: Matrix) : Matrix =
    List.map (fun l -> List.map f l) m
let map2 (op: Scalar->Scalar->Scalar) (a: Matrix) (b: Matrix) : Matrix =
    List.map2 (fun l1 l2 -> List.map2 op l1 l2) a b
let transpose (a: Matrix) : Matrix =
    match a with
        [] -> []
        | fst::rst ->
            List.foldBack (fun vec mat -> 
                List.map2 (@) (List.transpose [vec]) mat
            ) a (empty fst.Length)
let hconcat (a: Matrix) (b: Matrix) : Matrix =
    List.map2 (@) a b
let vconcat (a: Matrix) (b: Matrix) : Matrix =
    a @ b
let vec (a: Matrix) : Vector =
    foldBack (fun e acc -> e::acc) (transpose a) []
let mat (v: Vector) : Matrix =
    transpose [v]
let dot (a: Matrix) (b: Matrix) : Matrix =
    let c = transpose b
    List.map (fun v -> List.map (fun w -> List.map2 (*) v w |> List.sum) c ) a
let kroenecker (a: Matrix) (b: Matrix) : Matrix =
    match b with
        [] -> []
        | _ ->
            List.fold (fun (accRow: Matrix) (l: Vector) ->
                List.fold (fun (accCol: Matrix) (e: Scalar) -> 
                    map ((*) e) b
                    |> hconcat accCol
                ) (empty b.Length) l
                |> vconcat accRow
            ) [] a

let inline (+) a b = map2 (+) a b
