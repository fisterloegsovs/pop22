type Image<'a>(data : 'a [,]) =
  let _data = data

  new(rows, cols, c : 'a) =
    Image(Collections.Array2D.create<'a> rows cols c)
    
  new(rows, cols) =
    Image(Collections.Array2D.create<'a> rows cols Unchecked.defaultof<'a>)
 
  new() =
    Image(Collections.Array2D.create<'a> 256 256 Unchecked.defaultof<'a>)

  override this.ToString () =
    sprintf "%A" _data // this.data.ToString () gives a shorter result.

  member this.Item
    with get(a: int, b: int) = _data.[a, b]
    and set(a: int, b: int) (value: 'a) = _data.[a, b] <- value

  member this.ToArray2D =
    _data
    
  member this.rows =
    _data.GetLength(0)
    
  member this.cols =
    _data.GetLength(1)

  member this.GetSlice(rowStart: int option, rowFinish: int option, colStart: int option, colFinish: int option) =
    let rowStart =
      match rowStart with
        | Some(v) -> v
        | None -> 0
    let rowFinish =
      match rowFinish with
        | Some(v) -> v
        | None -> _data.GetLength(0) - 1
    let colStart =
      match colStart with
        | Some(v) -> v
        | None -> 0
    let colFinish =
      match colFinish with
        | Some(v) -> v
        | None -> _data.GetLength(1) - 1
    new Image<'a>(_data.[rowStart..rowFinish, colStart..colFinish])

  static member ( * ) (I1 : Image<float>, I2 : Image<float>) =
    let mul (I1 : Image<float>) (I2 : Image<float>) i j =
      I1.[i,j] * I2.[i,j]
    if not (I1.rows = I2.rows) || not (I1.cols = I2.cols) then
      failwith "The two images are not the same size"
    Collections.Array2D.init<float> I1.rows I1.cols (mul I1 I2)

  static member ( * ) (I1 : Image<float>, a : float) =
    let mul (I1 : Image<float>) (a : float) i j =
      I1.[i,j] * a
    Collections.Array2D.init<float> I1.rows I1.cols (mul I1 a)

  static member ( * ) (a : float, I2 : Image<float>) =
    I2 * a

  static member ( * ) (I1 : Image<System.Drawing.Color>, I2 : Image<System.Drawing.Color>) =
    let mul (I1 : Image<System.Drawing.Color>) (I2 : Image<System.Drawing.Color>) i j =
      let c1 = I1.[i,j]
      let c2 = I2.[i,j]
      System.Drawing.Color.FromArgb ((int c1.R) * (int c2.R), (int c1.G) * (int c2.G), (int c1.B) * (int c2.B))
    if not (I1.rows = I2.rows) || not (I1.cols = I2.cols) then
      failwith "The two images are not the same size"
    Collections.Array2D.init<System.Drawing.Color> I1.rows I1.cols (mul I1 I2)

  static member ( * ) (I1 : Image<System.Drawing.Color>, a : int) =
    let mul (I1 : Image<System.Drawing.Color>) (a : int) i j =
      let c1 = I1.[i,j]
      System.Drawing.Color.FromArgb ((int c1.R) * a, (int c1.G) * a, (int c1.B) * a)
    Collections.Array2D.init<System.Drawing.Color> I1.rows I1.cols (mul I1 a)

  static member ( * ) (a : int, I2 : Image<System.Drawing.Color>) =
    I2 * a

  static member ( * ) (I1 : Image<System.Drawing.Color>, c : System.Drawing.Color) =
    let mul (I1 : Image<System.Drawing.Color>) (c : System.Drawing.Color) i j =
      let c1 = I1.[i,j]
      System.Drawing.Color.FromArgb ((int c1.R) * (int c.R), (int c1.G) * (int c.G), (int c1.B) * (int c.B))
    Collections.Array2D.init<System.Drawing.Color> I1.rows I1.cols (mul I1 c)

  static member ( * ) (c : System.Drawing.Color, I2 : Image<System.Drawing.Color>) =
    I2 * c

  static member ( + ) (I1 : Image<float>, I2 : Image<float>) =
    let mul (I1 : Image<float>) (I2 : Image<float>) i j =
      I1.[i,j] + I2.[i,j]
    if not (I1.rows = I2.rows) || not (I1.cols = I2.cols) then
      failwith "The two images are not the same size"
    Collections.Array2D.init<float> I1.rows I1.cols (mul I1 I2)

  static member ( + ) (I1 : Image<float>, a : float) =
    let mul (I1 : Image<float>) (a : float) i j =
      I1.[i,j] + a
    Collections.Array2D.init<float> I1.rows I1.cols (mul I1 a)

  static member ( + ) (a : float, I2 : Image<float>) =
    I2 + a

  static member ( + ) (I1 : Image<System.Drawing.Color>, I2 : Image<System.Drawing.Color>) =
    let mul (I1 : Image<System.Drawing.Color>) (I2 : Image<System.Drawing.Color>) i j =
      let c1 = I1.[i,j]
      let c2 = I2.[i,j]
      System.Drawing.Color.FromArgb ((int c1.R) + (int c2.R), (int c1.G) + (int c2.G), (int c1.B) + (int c2.B))
    if not (I1.rows = I2.rows) || not (I1.cols = I2.cols) then
      failwith "The two images are not the same size"
    Collections.Array2D.init<System.Drawing.Color> I1.rows I1.cols (mul I1 I2)

  static member ( + ) (I1 : Image<System.Drawing.Color>, a : int) =
    let mul (I1 : Image<System.Drawing.Color>) (a : int) i j =
      let c1 = I1.[i,j]
      System.Drawing.Color.FromArgb ((int c1.R) + a, (int c1.G) + a, (int c1.B) + a)
    Collections.Array2D.init<System.Drawing.Color> I1.rows I1.cols (mul I1 a)

  static member ( + ) (a : int, I2 : Image<System.Drawing.Color>) =
    I2 + a

  static member ( + ) (I1 : Image<System.Drawing.Color>, c : System.Drawing.Color) =
    let mul (I1 : Image<System.Drawing.Color>) (c : System.Drawing.Color) i j =
      let c1 = I1.[i,j]
      System.Drawing.Color.FromArgb ((int c1.R) + (int c.R), (int c1.G) + (int c.G), (int c1.B) + (int c.B))
    Collections.Array2D.init<System.Drawing.Color> I1.rows I1.cols (mul I1 c)

  static member ( + ) (c : System.Drawing.Color, I2 : Image<System.Drawing.Color>) =
    I2 + c

// Test creation of an image
let F1 = new Image<float>(3,5,9.0)
printfn "F1:\n%A" F1
let F2 = new Image<float>(3,5)
printfn "F2:\n%A" F2
let F3 = new Image<float>()
printfn "F3:\n%A" F3
let arr = Collections.Array2D.init<float> 3 5 (fun i j -> float (i*5+j)) 
let F4 = new Image<float>(arr)
printfn "F4:\n%A" F4
let c = System.Drawing.Color.FromArgb(0,1,2)
let C1 = new Image<System.Drawing.Color>(3,5,c)
printfn "C1:\n%A" C1
let C2 = new Image<System.Drawing.Color>(3,5)
printfn "C2:\n%A" C2

// Test indexing
printfn "F4.[1,2]\n%A" F4.[1,2]
F4.[1,2] <- 3.0
printfn "F4.[1,2]\n%A" F4.[1,2]
printfn "F4.[0..1,1..2]\n%A" F4.[0..1,1..2]
printfn "F4.[..1,1..2]\n%A" F4.[..1,1..2]
printfn "F4.[0..,1..2]\n%A" F4.[0..,1..2]
printfn "F4.[1..1,..2]\n%A" F4.[1..1,..2]
printfn "F4.[1..1,1..]\n%A" F4.[1..1,1..]

// Testing multiplication
printfn "3.0 * F4:\n%A" (3.0 * F4)
printfn "F3 * F4:\n%A" (F1 * F4)
printfn "3 * C1:\n%A" (3 * C1)
printfn "System.Drawing.Color.FromArgb(2,2,2) * C1:\n%A" (System.Drawing.Color.FromArgb(2,2,2) * C1)
printfn "C2 * C1:\n%A" (C2 * C1)

// Testing addition
printfn "3.0 + F4:\n%A" (3.0 + F4)
printfn "F3 + F4:\n%A" (F1 + F4)
printfn "3 + C1:\n%A" (3 + C1)
printfn "System.Drawing.Color.FromArgb(2,2,2) + C1:\n%A" (System.Drawing.Color.FromArgb(2,2,2) + C1)
printfn "C2 + C1:\n%A" (C2 + C1)
