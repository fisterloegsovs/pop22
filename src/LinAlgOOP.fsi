module LinAlgOOP
type Matrix =
  class
    new : int * int -> Matrix
    new : a2DArray:float [,] -> Matrix

    member cols : int
    member rows : int

    member Item : a:int * b:int -> float with get
    member Item : a:int * b:int -> float with set

    member GetSlice : row:int * colStart:int option * colFinish:int option -> Matrix
    member GetSlice : rowStart:int option * rowFinish:int option * col:int -> Matrix
    member GetSlice : rowStart:int option * rowFinish:int option * colStart:int option * colFinish:int option -> Matrix

    member ToArray : float [,]

/// <summary>Concatenate to matrices along the row direction.</summary>
/// <param name="other">A matrix, must have same numbre of columns as this.</param>
/// <returns>A new matrix.</returns>
/// <exception cref="System.ArgumentException">Thrown when the two matrices have different number of columns.</exception>
    member RowConcat : other:Matrix -> Matrix

/// <summary>Concatenate to matrices along the column direction.</summary>
/// <param name="other">A matrix, must have same numbre of rows as this.</param>
/// <returns>A new matrix.</returns>
/// <exception cref="System.ArgumentException">Thrown when the two matrices have different number of rows.</exception>
    member ColConcat : other:Matrix -> Matrix


/// <summary>Make a new matrix whos result is the sum of this and its argument.</summary>
/// <param name="other">A matrix, must have same size as this.</param>
/// <returns>A matrix of same size as this.</returns>
/// <exception cref="System.ArgumentException">Thrown when the two matrices are of different sizes.</exception>
    member Add : other:Matrix -> Matrix

    member Mul : a:float -> Matrix
    member Mul : other:Matrix -> Matrix

    /// We solve the following system using Cramer's rule
    /// A x = b
    member Cramer : b:Matrix -> Matrix

    /// We solve for the inverse by use Cramer's rule
    member Inverse : unit -> Matrix
    member Minor : i:int * j:int -> Matrix
    member Det : float
    member Transpose : Matrix
    static member Id : rows:int -> Matrix

    /// Generate a uniformly distributed random matrix.
    static member Random : rows:int -> cols:int -> Matrix
  end
