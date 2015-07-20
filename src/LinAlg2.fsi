module LinAlg
[<Sealed>]
type Vector =
  static member ( ~- ) : Vector -> Vector
  static member ( + )  : Vector * Vector -> Vector
  static member ( - )  : Vector * Vector -> Vector
  static member ( * )  : float  * Vector -> Vector
  static member ( * )  : Vector * Vector -> float
val make : float list -> Vector
val coord: Vector -> float list
val norm : Vector -> float
    
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
