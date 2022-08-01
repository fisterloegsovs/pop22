module linalg
type Scalar = float
type Vector = Scalar list
type Matrix(m: Vector list) =
    member this.lstlst = m
    member this.isMatrix : bool =
        match m with
            [] -> true
            | fst::rst -> 
                let col = fst.Length
                List.forall (fun (l: Vector) -> l.Length = col) m
    member this.size : int*int =
        (m.Length, m.Head.Length)
    override this.ToString() =
        List.foldBack (fun elm acc ->
            let elmStr =
                List.foldBack (fun e a -> sprintf "%A " e + a) elm ""
            sprintf "%s\n" elmStr + acc
            ) this.lstlst ""

    static member map2 (op: Scalar->Scalar->Scalar) (a: Matrix) (b: Matrix) : Matrix =
        Matrix(List.map2 (fun l1 l2 -> List.map2 op l1 l2) a.lstlst b.lstlst)
    // operator overloading: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/operator-overloading
    static member (+) (a: Matrix, b: Matrix) =
        Matrix.map2 (+) a b
    static member (*) (a: Matrix, b: Matrix) =
        Matrix.map2 (*) a b
    static member (-) (a: Matrix, b: Matrix) =
        Matrix.map2 (-) a b
    // unary minus
    static member map (f: Scalar->Scalar) (a: Matrix) : Matrix =
        Matrix(List.map (fun l -> List.map f l) a.lstlst)
    static member (~-) a = Matrix.map (~-) a

    static member (+) (a: Scalar, b: Matrix) =
        Matrix.map (fun e -> a*e) b
    static member (+) (b: Matrix, a: Scalar) =
        a+b
