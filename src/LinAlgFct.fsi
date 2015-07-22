module LinAlgFct
[<Sealed>]
type Vector =
  | V of float list
  with
    static member ( + ) : Vector * Vector -> Vector
    static member ( * ) : a:float * Vector -> Vector
    static member ( * ) : Vector * Vector -> float
    static member ( - ) : Vector * Vector -> Vector
    static member ( ~- ) : Vector -> Vector
  end
val length : Vector -> int
val VectorGetSlice : Vector * rowStart:int * rowFinish:int -> Vector
val VectorNorm : Vector -> float
val VectorToList : Vector -> float list
val VectorToString : Vector -> string
val VectorOne : int -> Vector
val VectorRandom : int -> Vector
    
type Matrix =
  | M of float list list
  with
    static member ( + ) : Matrix * Matrix -> Matrix
    static member ( * ) : a:float * Matrix -> Matrix
    static member ( * ) : Matrix * Matrix -> Matrix
    static member ( * ) : Matrix * Vector -> Matrix
    static member ( * ) : Vector * Matrix -> Matrix
    static member ( - ) : M1:int * M2:int -> int
    static member ( ~- ) : Matrix -> Matrix
  end
val rows : Matrix -> int
val cols : Matrix -> int
val transpose : Matrix -> Matrix
val VectorToMatrix : Vector -> Matrix
val MatrixToVector : Matrix -> Vector
val MatrixGetSlice : Matrix * rowStart:int * rowFinish:int * colStart:int * colFinish:int -> Matrix
val RowConcat : Matrix * Matrix -> Matrix
val ColConcat : Matrix * Matrix -> Matrix
val Minor : Matrix * int * int -> Matrix
val Det : Matrix -> float
val Cramer : Matrix * Vector -> Vector
val Inverse : Matrix -> Matrix
val Kronecker : Matrix * Matrix -> Matrix
val MatrixToListList : Matrix -> float list list
val MatrixToString : Matrix -> string
val trace : Matrix -> float
val id : int -> Matrix
val MatrixRandom : int * int -> Matrix
