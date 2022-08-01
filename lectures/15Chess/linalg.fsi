module linalg
type Scalar = int
type Vector = Scalar list
type Matrix = Vector list
val isMatrix : a:Matrix -> bool
val eye : n:int ->  Matrix
val empty : m:int -> Matrix
val size : a:Matrix -> int*int
val fold : f:('a -> Scalar -> 'a) -> n:'a -> a:Matrix -> 'a
val foldBack : f:(Scalar -> 'a -> 'a) -> a:Matrix -> n:'a -> 'a
val echo : a:Matrix -> unit
val forall : f:(Scalar->bool) -> a:Matrix -> bool
val forall2 : f:(Scalar->Scalar->bool) -> a:Matrix -> b:Matrix -> bool
val map : f:(Scalar->Scalar) -> m:Matrix -> Matrix 
val map2 : op:(Scalar->Scalar->Scalar) -> a:Matrix -> b:Matrix -> Matrix
val transpose : a:Matrix -> Matrix
val hconcat : a:Matrix -> b:Matrix -> Matrix
val vconcat : a:Matrix -> b:Matrix -> Matrix
val vec : a:Matrix -> Vector
val mat : v:Vector -> Matrix
val dot : a:Matrix -> b:Matrix -> Matrix
val kroenecker : a:Matrix -> b:Matrix -> Matrix

val inline (+) : a:Matrix -> b:Matrix ->  Matrix
