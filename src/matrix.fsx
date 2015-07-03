type Matrix<'T>(N: int, M: int) =
    let _array = Array2D.zeroCreate<'T> N M

    member this.Item
        with get(a: int, b: int) = _array.[a, b]
        and set(a: int, b: int) (value:'T) = _array.[a, b] <- value

    member this.GetSlice(rowStart: int option, rowFinish : int option,
                         colStart: int option, colFinish : int option) =
           let rowStart = match rowStart with
                          | Some(v) -> v
                          | None -> 0
           let rowFinish = match rowFinish with
                           | Some(v) -> v
                           | None -> _array.GetLength(0) - 1
           let colStart = match colStart with
                          | Some(v) -> v
                          | None -> 0
           let colFinish = match colFinish with
                           | Some(v) -> v
                           | None -> _array.GetLength(1) - 1
           _array.[rowStart..rowFinish, colStart..colFinish]

    member this.GetSlice(row: int, colStart: int option, colFinish: int option) =
           let colStart = match colStart with
                          | Some(v) -> v
                          | None -> 0
           let colFinish = match colFinish with
                           | Some(v) -> v
                           | None -> _array.GetLength(1) - 1
           _array.[row, colStart..colFinish]

    member this.GetSlice(rowStart: int option, rowFinish: int option, col: int) =
           let rowStart = match rowStart with
                          | Some(v) -> v
                          | None -> 0
           let rowFinish = match rowFinish with
                           | Some(v) -> v
                           | None -> _array.GetLength(0) - 1
           _array.[rowStart..rowFinish, col]

module test =

    let generateTestMatrix x y =
        let matrix = new Matrix<float>(3, 3);
        for i in 0..2 do
           for j in 0..2 do
               matrix.[i, j] <- float(i) * x - float(j) * y
        matrix

    let test1 = generateTestMatrix 2.3 1.1
    let submatrix = test1.[0..1, 0..1]
    printfn "%A" submatrix

    let firstRow = test1.[0,*]
    let secondRow = test1.[1,*]
    let firstCol = test1.[*,0]
    printfn "%A" firstCol
    
