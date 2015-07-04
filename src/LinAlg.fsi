namespace LinAlg
module LinAlg =
  type Vector
    val norm  : Vector -> float // Length of vector
    val make  : float * float -> Vector // Make vector
    val coord : Vector -> float * float // Get coordinates
    val ( +. ) : Vector -> Vector -> Vector // Vector addition
    val ( -. ) : Vector -> Vector -> Vector // Vector subtraction
    val ( ~-. ) : Vector -> Vector // Vector negation
    val ( *. ) : float -> Vector -> Vector // scalar-Vector multiplication
    val ( &. ) : Vector -> Vector -> float // Vector dot product

  type Matrix = class
    new : a2DArray:float [,] -> Matrix
    new : rows:int * cols:int -> Matrix
    val private _array: float [,]
    member Item : a:int * b:int -> float with get
    member Item : a:int * b:int -> float with set
    member private Copy : a:float [,] * ?row:int * ?col:int -> Matrix
    member private CopyCol : a:float [] * ?row:int * ?col:int -> Matrix
    member private CopyRow : a:float [] * ?row:int * ?col:int -> Matrix
    member cols : int
    member rows : int
    member ToArray : float [,]
    member ColConcat : other:Matrix -> Matrix
    member RowConcat : other:Matrix -> Matrix
    member Add : other:Matrix -> Matrix
    member Mul : a:float -> Matrix
    member Mul : other:Matrix -> Matrix
    member Cramer : b:Matrix -> Matrix
    member GetSlice : row:int * colStart:int option * colFinish:int option -> Matrix
    member GetSlice : rowStart:int option * rowFinish:int option * col:int -> Matrix
    member GetSlice : rowStart:int option * rowFinish:int option * colStart:int option * colFinish:int option -> Matrix
    member Inverse : unit -> Matrix
    member Minor : i:int * j:int -> Matrix
    member Det : float
    member Transpose : Matrix
    static member Id : rows:int -> Matrix
    static member Random : rows:int -> cols:int -> Matrix
  end
