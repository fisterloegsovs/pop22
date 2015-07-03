type Matrix(M: int, N: int) =
  let _array = Array2D.zeroCreate M N

  member this.Item
    with get(a: int, b: int) = _array.[a, b]
    and set(a: int, b: int) (value:float) = _array.[a, b] <- value

  member this.Copy(a: float[,], ?row: int, ?col: int) =
    let row = 
      match row with
        | Some(v) -> v
        | None -> 0
    let col = 
      match col with
        | Some(v) -> v
        | None -> 0
    for i in 0..a.GetLength(0)-1 do
      for j in 0..a.GetLength(1)-1 do
        _array.[row+i,col+j] <- a.[i,j]
    this

  member this.CopyCol(a: float[], ?row: int, ?col: int) =
    let row = 
      match row with
        | Some(v) -> v
        | None -> 0
    let col = 
      match col with
        | Some(v) -> v
        | None -> 0
    for i in 0..a.GetLength(0)-1 do
      _array.[row+i,col] <- a.[i]
    this

  member this.CopyRow(a: float[], ?row: int, ?col: int) =
    let row = 
      match row with
        | Some(v) -> v
        | None -> 0
    let col = 
      match col with
        | Some(v) -> v
        | None -> 0
    for j in 0..a.GetLength(0)-1 do
      _array.[row,col+j] <- a.[j]
    this

  member this.GetSlice(rowStart: int option, rowFinish: int option, colStart: int option, colFinish: int option) =
    let rowStart =
      match rowStart with
        | Some(v) -> v
        | None -> 0
    let rowFinish =
      match rowFinish with
        | Some(v) -> v
        | None -> _array.GetLength(0) - 1
    let colStart =
      match colStart with
        | Some(v) -> v
        | None -> 0
    let colFinish =
      match colFinish with
        | Some(v) -> v
        | None -> _array.GetLength(1) - 1
    let rows = rowFinish-rowStart+1
    let cols = colFinish-colStart+1
    let slice = new Matrix(rows,cols)
    slice.Copy(_array.[rowStart..rowFinish, colStart..colFinish])

  member this.GetSlice(row: int, colStart: int option, colFinish: int option) =
    let colStart =
      match colStart with
        | Some(v) -> v
        | None -> 0
    let colFinish =
      match colFinish with
        | Some(v) -> v
        | None -> _array.GetLength(1) - 1
    let slice = new Matrix(1,colFinish-colStart+1)
    slice.CopyRow(_array.[row, colStart..colFinish])

  member this.GetSlice(rowStart: int option, rowFinish: int option, col: int) =
    let rowStart =
      match rowStart with
        | Some(v) -> v
        | None -> 0
    let rowFinish =
      match rowFinish with
        | Some(v) -> v
        | None -> _array.GetLength(0) - 1
    let slice = new Matrix(rowFinish-rowStart+1,1)
    slice.CopyCol(_array.[rowStart..rowFinish, col])

  member this.ToArray =
    _array
    
  member this.rows =
    M
    
  member this.cols =
    N

  member this.RowConcat(other: Matrix) =
    let result = Matrix(M,N+other.cols)
    result.Copy(this.ToArray) |> ignore
    result.Copy(other.ToArray, 0, N) |> ignore
    result
        
  member this.ColConcat(other: Matrix) =
    let result = Matrix(M+other.rows,N)
    result.Copy(this.ToArray) |> ignore
    result.Copy(other.ToArray, M, 0) |> ignore
    result

  member this.Minor(i:int, j:int) =
    let result = Matrix(M-1,N-1)
    if i>0 && j>0 then result.Copy(_array.[0..i-1,0..j-1]) |> ignore
    if i>0 && j<N-1 then result.Copy(_array.[0..i-1,j+1..N-1],0,j) |> ignore
    if i<M && j>0 then result.Copy(_array.[i+1..M-1,0..j-1],i,0) |> ignore
    if i<M && j<N-1 then result.Copy(_array.[i+1..M-1,j+1..N-1],i,j) |> ignore
    result

  member this.Det =
    let mutable sum = 0.0
    if this.rows = 2 then
      sum <- _array.[0,0]*_array.[1,1]-_array.[0,1]*_array.[1,0]
    else
      for j in 0..N-1 do
        let minor = this.Minor(0,j)
        let minorDet = minor.Det
        let cofactor = (-1.0**float j)*_array.[0,j]*minorDet
        sum <- sum+cofactor
    sum
      
  static member Id(N:int) =
    let result = new Matrix(N,N)
    for i in 0..N-1 do
      result.[i,i] <- 1.0
    result

  member this.Transpose =
    let result = new Matrix(N,M)
    for i in 0..M-1 do
      for j in 0..N-1 do
        result.[j,i] <- _array.[i,j]
    result

  member this.Add(other: Matrix) =
    let result = Matrix(M,N)
    for i in 0..M-1 do
      for j in 0..N-1 do
        result.[i,j] <- _array.[i,j] + other.[i,j]
    result
        
  member this.Mul(a: float) =
    let result = Matrix(M,N)
    for i in 0..M-1 do
      for j in 0..N-1 do
        result.[i,j] <- _array.[i,j]*a
    result

  member this.Mul(other: Matrix) =
    let result = Matrix(M,other.cols)
    for i in 0..M-1 do
      for j in 0..other.cols-1 do
        result.[i,j] <- 0.0;
        for k in 0..N-1 do
          result.[i,j] <- result.[i,j] + _array.[i,k]*other.[k,j]
    result

module test =

  let generateTestMatrix (x:float) (y:float) =
    let matrix = new Matrix(3, 4)
    for i in 0..matrix.rows-1 do
      for j in 0..matrix.cols-1 do
        matrix.[i, j] <- float(i) * x - float(j) * y
    matrix

  let print (s:string) (a:Matrix) =
    printfn "%s: %d x %d =\n%A" s a.rows a.cols a.ToArray
      
  let test1 = generateTestMatrix 2.3 1.1
  print "test1" test1

  let submatrix = test1.[0..1, 0..1]
  print "test1.[0..1, 0..1]" submatrix

  let firstRow = test1.[0,*]
  print "test1.[0,*]" firstRow

  let secondRow = test1.[1,*]
  print "test1.[1,*]" secondRow

  let firstCol = test1.[*,0]
  print "test1.[*,0]" firstCol

  let test2 = test1.Transpose
  print "test1.Transpose" test2

  let I = Matrix.Id(3)
  print "I" I

  let J = test1.Add(test1)
  print "test1+test1" J

  let K = I.Mul(test1)
  print "I*test1" K

  let P = test1.Mul(test1.Transpose)
  print "test1*(test1.Transpose)" P

  let Q = test1.RowConcat(test1)
  print "test1.RowConcat(test1)" Q

  let R = test1.ColConcat(test1)
  print "test1*(test1.ColConcat(test1)" R

  print "Minors of test1" test1
  for i in 0..test1.rows-1 do
    for j in 0..test1.cols-1 do
      let str = sprintf "%d, %d" i j
      print str (test1.Minor(i,j))

  printfn "I.Det = %f" I.Det
