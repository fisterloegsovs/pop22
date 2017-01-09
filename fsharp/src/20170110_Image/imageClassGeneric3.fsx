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

  // Matrix operator basics
  static member ImageOppImage (opp : Image<'a> -> Image<'b> -> int -> int -> 'c) (I1 : Image<'a>) (I2 : Image<'b>) =
    if not (I1.rows = I2.rows) || not (I1.cols = I2.cols) then
      failwith "The two images are not the same size"
    Image<'c>(Collections.Array2D.init<'c> I1.rows I1.cols (opp I1 I2))

  static member ImageOppConst (opp : Image<'a> -> 'b -> int -> int -> 'c) (I1 : Image<'a>) (a : 'b) =
    Image<'c>(Collections.Array2D.init<'c> I1.rows I1.cols (opp I1 a))

  static member OppImage (opp : Image<'a> -> int -> int -> 'b) (I1 : Image<'a>) =
    Image<'b>(Collections.Array2D.init<'b> I1.rows I1.cols (opp I1))

  // Multiplication
  static member ( * ) (I1 : Image<float>, I2 : Image<float>) =
    Image.ImageOppImage (fun I1 I2 i j -> I1.[i,j] * I2.[i,j]) I1 I2

  static member ( * ) (I1 : Image<float>, a : float) =
    Image.ImageOppConst (fun I1 a i j -> I1.[i,j] * a) I1 a

  static member ( * ) (a : float, I1 : Image<float>) =
    I1 * a

  static member ( * ) (I1 : Image<System.Drawing.Color>, I2 : Image<System.Drawing.Color>) =
    let mul (I1 : Image<System.Drawing.Color>) (I2 : Image<System.Drawing.Color>) i j =
      let c1 = I1.[i,j]
      let c2 = I2.[i,j]
      System.Drawing.Color.FromArgb ((int c1.R) * (int c2.R), (int c1.G) * (int c2.G), (int c1.B) * (int c2.B))
    Image.ImageOppImage mul I1 I2

  static member ( * ) (I1 : Image<System.Drawing.Color>, c : System.Drawing.Color) =
    let mul (I1 : Image<System.Drawing.Color>) (c : System.Drawing.Color) i j =
      let c1 = I1.[i,j]
      System.Drawing.Color.FromArgb ((int c1.R) * (int c.R), (int c1.G) * (int c.G), (int c1.B) * (int c.B))
    Image.ImageOppConst mul I1 c

  static member ( * ) (c : System.Drawing.Color, I1 : Image<System.Drawing.Color>) =
    I1 * c
    
  static member ( * ) (I1 : Image<System.Drawing.Color>, a : int) =
    let mul (I1 : Image<System.Drawing.Color>) (a : int) i j =
      let c1 = I1.[i,j]
      System.Drawing.Color.FromArgb ((int c1.R) * a, (int c1.G) * a, (int c1.B) * a)
    Image.ImageOppConst mul I1 a

  static member ( * ) (a : int, I1 : Image<System.Drawing.Color>) =
    I1 * a

  // Addition
  static member ( + ) (I1 : Image<float>, I2 : Image<float>) =
    Image.ImageOppImage (fun I1 I2 i j -> I1.[i,j] + I2.[i,j]) I1 I2

  static member ( + ) (I1 : Image<float>, a : float) =
    Image.ImageOppConst (fun I1 a i j -> I1.[i,j] + a) I1 a

  static member ( + ) (a : float, I1 : Image<float>) =
    I1 + a

  static member ( + ) (I1 : Image<System.Drawing.Color>, I2 : Image<System.Drawing.Color>) =
    let mul (I1 : Image<System.Drawing.Color>) (I2 : Image<System.Drawing.Color>) i j =
      let c1 = I1.[i,j]
      let c2 = I2.[i,j]
      System.Drawing.Color.FromArgb ((int c1.R) + (int c2.R), (int c1.G) + (int c2.G), (int c1.B) + (int c2.B))
    Image.ImageOppImage mul I1 I2

  static member ( + ) (I1 : Image<System.Drawing.Color>, a : System.Drawing.Color) =
    let mul (I1 : Image<System.Drawing.Color>) (a : System.Drawing.Color) i j =
      let c1 = I1.[i,j]
      System.Drawing.Color.FromArgb ((int c1.R) + (int a.R), (int c1.G) + (int a.G), (int c1.B) + (int a.B))
    Image.ImageOppConst mul I1 a

  static member ( + ) (a : System.Drawing.Color, I1 : Image<System.Drawing.Color>) =
    I1 + a

  static member ( + ) (I1 : Image<System.Drawing.Color>, a : int) =
    let mul (I1 : Image<System.Drawing.Color>) (a : int) i j =
      let c1 = I1.[i,j]
      System.Drawing.Color.FromArgb ((int c1.R) + a, (int c1.G) + a, (int c1.B) + a)
    Image.ImageOppConst mul I1 a

  static member ( + ) (a : int, I1 : Image<System.Drawing.Color>) =
    I1 + a

  // Unary minus (not defined for Color)
  static member ( ~- ) (I1 : Image<float>) =
    Image.OppImage (fun I1 i j -> -I1.[i,j]) I1

  // Subtraction
  static member ( - ) (I1 : Image<float>, I2 : Image<float>) =
    Image.ImageOppImage (fun I1 I2 i j -> I1.[i,j] - I2.[i,j]) I1 I2

  static member ( - ) (I1 : Image<float>, a : float) =
    Image.ImageOppConst (fun I1 a i j -> I1.[i,j] - a) I1 a

  static member ( - ) (a : float, I1 : Image<float>) =
    I1 - a

  static member ( - ) (I1 : Image<System.Drawing.Color>, a : System.Drawing.Color) =
    let mul (I1 : Image<System.Drawing.Color>) (a : System.Drawing.Color) i j =
      let c1 = I1.[i,j]
      System.Drawing.Color.FromArgb ((int c1.R) - (int a.R), (int c1.G) - (int a.G), (int c1.B) - (int a.B))
    Image.ImageOppConst mul I1 a

  static member ( - ) (a : System.Drawing.Color, I1 : Image<System.Drawing.Color>) =
    let mul (I1 : Image<System.Drawing.Color>) (a : System.Drawing.Color) i j =
      let c1 = I1.[i,j]
      System.Drawing.Color.FromArgb ((int a.R) - (int c1.R), (int a.G) - (int c1.G), (int a.B) - (int c1.B))
    Image.ImageOppConst mul I1 a

  static member ( - ) (I1 : Image<System.Drawing.Color>, I2 : Image<System.Drawing.Color>) =
    let mul (I1 : Image<System.Drawing.Color>) (I2 : Image<System.Drawing.Color>) i j =
      let c1 = I1.[i,j]
      let c2 = I2.[i,j]
      System.Drawing.Color.FromArgb ((int c1.R) - (int c2.R), (int c1.G) - (int c2.G), (int c1.B) - (int c2.B))
    Image.ImageOppImage mul I1 I2

  static member ( - ) (I1 : Image<System.Drawing.Color>, a : int) =
    let mul (I1 : Image<System.Drawing.Color>) (a : int) i j =
      let c1 = I1.[i,j]
      System.Drawing.Color.FromArgb ((int c1.R) - a, (int c1.G) - a, (int c1.B) - a)
    Image.ImageOppConst mul I1 a


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

// Testing unary minus
printfn "-F4:\n%A" (-F4)

// Testing subtraction
printfn "3.0 - F4:\n%A" (3.0 - F4)
printfn "F3 - F4:\n%A" (F1 - F4)
printfn "System.Drawing.Color.FromArgb(10,10,10) - C1:\n%A" (System.Drawing.Color.FromArgb(10,10,10) - C1)
printfn "C1 - C2:\n%A" (C1 - C2)
