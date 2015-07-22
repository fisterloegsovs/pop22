//module LinAlg
(*
let rec zip = function
  | ([],_)
  | (_,[]) -> []
  | (x::xs,y::ys) -> (x,y)::(zip (xs, ys))

let rec zipzip = function
  | ([],_)
  | (_,[]) -> []
  | (xs::xss,ys::yss) -> (zip (xs,ys))::(zipzip (xss, yss))
*)

let rec rconc = function
  | ((xs::xss),(ys::yss)) -> (xs@ys)::rconc(xss,yss)
  | ([],yss) -> yss
  | (xss,[]) -> xss

let rec tr = function
  | (_::_)::_ as M -> (List.map List.head M) :: (tr (List.map List.tail M))
  | _ -> []

let rec add = function
  | (x::xs,y::ys) -> (x+y)::add (xs,ys)
  | _ -> []

let rec addadd = function
  | (xs::xss,ys::yss) -> (add (xs,ys))::(addadd (xss,yss))
  | _ -> []

let rec dot = function
  | (x::xs,y::ys) -> x*y + (dot (xs,ys))
  | _ -> 0.0

let constMul a xs = List.map (fun x -> a*x ) xs

let mulmul (xss,yss) =
  let dotNAppendFct xs = (fun ys dotList -> (dot (xs,ys))::dotList)
  let dotAllNAppendFct = (fun xs dotAllList -> (List.foldBack (dotNAppendFct xs)  (tr yss) [])::dotAllList)
  List.foldBack dotAllNAppendFct xss []

let rec listSkip n xs = 
  match (n, xs) with
  | (0, _) -> xs
  | (_, []) -> []
  | (n, _::xs) -> listSkip (n-1) xs

let rec listTake n xs = 
  match (n, xs) with
  | (0, _) -> []
  | (_, []) -> []
  | (n, x::xs) -> x::listTake (n-1) xs

let rec listRemove n xs = 
  match (n, xs) with
  | (_, []) -> []
  | (0, x::xs) -> listRemove (n-1) xs
  | (n, x::xs) -> x::(listRemove (n-1) xs)

let rec listReplace n y xs = 
  match (n, y, xs) with
  | (_, _, []) | (_, [], _) -> []
  | (0, y, x::xs) -> y::(listReplace (n-1) y xs)
  | (n, y, x::xs) -> x::(listReplace (n-1) y xs)

let unit len i =
  [for j in 1..i-1 -> 0.0]@[1.0]@[for j in i+1..len -> 0.0]

type Vector = V of float list
let length (V(xs)) = List.length xs
type Vector with
  static member ( * )  (a, V(xs)) = V(constMul a xs )
  static member ( + ) (V(xs),V(ys)) =
    if length (V(xs)) = length (V(ys)) then
      V(add (xs,ys))
    else
      failwith "Sum of 2 vectors of unequal lengths is undefined"
  static member ( * )  (V(xs), V(ys)) =
    if length (V(xs)) = length (V(ys)) then
      dot (xs,ys)
    else
      failwith "Product of 2 vectors of unequal lengths is undefined"
  static member ( ~- ) (V(xs)) = (-1.0)*V(xs)
  static member ( - ) (V(xs),V(ys)) = V(xs) + (- V(ys))
let VectorGetSlice (V(xs), rowStart, rowFinish) = V(xs |> listSkip rowStart |> listTake (rowFinish-rowStart+1))
let VectorNorm (V(xs)) = sqrt(V(xs) * V(xs))
let VectorToList (V(xs)) = xs
let VectorToString = function
  | (V(x::xs)) ->
      let commaConcatFct = fun e x -> e+", "+(string x)
      "[ " + (string x) + (List.fold commaConcatFct "" xs) + " ]"
  | V([]) -> "[]"
let VectorOne = function
  | t when t > 0 ->
      V([for i in 1..t -> 1.0])
  | _ -> V([])
let VectorRandom = function
  | len when len >0 ->
      let rnd = System.Random()
      V([for j in 1..len -> rnd.NextDouble()])
  | _ -> V([])

type Matrix = M of float list list
let rows (M(xss)) = List.length xss
let cols = function
  | M(xs::xss) -> List.length xs
  | M([]) -> 0
let transpose (M(xss)) =
  M(tr xss)
let VectorToMatrix (V(xs)) = transpose (M([xs]))
let MatrixToVector = function
  | M([]) -> V([])
  | M(xs::[]) -> V(xs)
  | M((x::[])::_ as xss) ->
      let xss' = tr xss
      V(List.head xss')
  | _ -> failwith "A matrix with more than 1 row cannot be converted to a vector"
type Matrix with
  static member ( * )  (a, M(xss)) = M(List.map (constMul a) xss)
  static member ( + ) (M(xss),M(yss)) =
    if (rows (M(xss)) = rows (M(yss))) && (cols (M(xss)) = cols (M(yss))) then
      M(addadd (xss,yss))
    else
      failwith "Sum of 2 matrices of unequal sizes is undefined"
  static member ( * )  (M(xss), M(yss)) =
    if cols (M(xss)) = rows (M(yss)) then
      M(mulmul (xss,yss))
    else
      failwith "Product of matrices A in R^{nxm} and B in R^{pxq} is not defined when m <> p"
  static member ( * )  (M(xss), V(ys)) =
    if cols (M(xss)) = length (V(ys)) then
      M(xss) * (VectorToMatrix (V(ys)))
    else
      failwith "The number of matrix columns does not match the vector length"
  static member ( * )  (V(ys), M(xss)) =
    if rows (M(xss)) = length (V(ys)) then
      (VectorToMatrix (V(ys))) * M(xss)
    else
      failwith "The vector length does not match the number of matrix rows"
  static member ( ~- ) (M(xss)) = (-1.0)*M(xss)
  static member ( - ) (M1,M2) = M1 + (- M2)
let MatrixGetSlice (M(xss), rowStart, rowFinish, colStart, colFinish) =
  let xss' = xss |> listSkip rowStart |> listTake (rowFinish-rowStart+1)
  M(List.map (fun xs -> xs |> listSkip colStart |> listTake (colFinish-colStart+1)) xss')
let RowConcat = function
  | (M([]),M([])) ->  M([])
  | (M(xss),M([])) -> M(xss)
  | (M([]),M(yss)) -> M(yss)
  | (M(xss),M(yss)) when rows (M(xss)) = rows (M(yss)) -> M(rconc (xss,yss))
  | _ -> failwith "The matrices do not have the same number of rows"
let ColConcat = function
  | (M([]),M([])) ->  M([])
  | (M(xss),M([])) -> M(xss)
  | (M([]),M(yss)) -> M(yss)
  | (M(xss),M(yss)) when cols (M(xss)) = cols (M(yss)) -> M(xss@yss)
  | _ -> failwith "The matrices do not have the same number of cols"
let Minor (M(xss),i,j) =
  M(listRemove i (List.map (fun xs -> listRemove j xs) xss))
let rec Det = function
  | M([]) -> 1.0
  | M(xs::_ as xss) ->
    let rec _det = function
      | (j,x::xs,xss) when j <= cols (M(xss))->
        x*(Det (Minor (M(xss), 0, j)))-_det (j+1,xs,xss)
      | _ -> 0.0
    _det(0,xs,xss)
let Cramer = function
  | (M(xss),V(xs)) when cols (M(xss)) = length (V(xs)) ->
    let detM = Det (M(xss))
    let rec _cramer = function
      | (j,trXss,xs) when j < List.length xs ->
          let xssj = tr (listReplace j xs trXss)
          ((Det (M(xssj)))/detM)::_cramer (j+1, trXss, xs)
      | _ -> []
    V(_cramer (0, (tr xss), xs))
  | _ -> failwith "The number of columns in M does not match the length of the vector V"
let Inverse = function
  | M(xss) when (cols (M(xss)) = rows (M(xss))) ->
    let len = rows (M(xss))
    let unitFct = unit len
    M(tr [for i in 1..len -> Cramer (M(xss), V(unitFct i)) |> VectorToList])
  | _ -> failwith "This inverse is only defined for square matrices"
let Kronecker = function
  | (M([]),_) | (_,M([])) -> M([])
  | (M(xs:_ as xss),M(yss)) ->
    let rec _mul = function
      | ([],_) | (_,[]) -> []
      | (a::xs',yss') ->
        let aYss = List.map (constMul a) yss'
        let rest = _mul (xs', yss')
        rconc (aYss, rest)
    let _rowKronecker = fun xs' e -> (_mul (xs', yss))@e
    M(List.foldBack _rowKronecker xss [])
let MatrixToListList (M(xss)) = xss
let MatrixToString = function
  | (M(xs::xss)) ->
      let commaConcatFct = fun e xs -> e+", "+(VectorToString (V(xs)))
      "[ " + (VectorToString (V(xs))) + (List.fold commaConcatFct "" xss) + " ]"
  | (M([])) -> "[[]]"
let trace = function
  | M(xss) ->
    let rec _trace = function
      | (x::[])::[] -> x
      | (x::xs)::xss -> x + _trace(List.map (fun xs -> List.tail xs) xss)
      | _ -> failwith "Trace of an empty matrix is undefined"
    _trace xss
let id = function
  | t when t > 0 ->
    M([for i in 1..t -> (unit t i)])
  | _ -> failwith "Identity matrix of non postive size is undefined"
let MatrixRandom = function
  | (rows,cols) when ((rows>0) && (cols>0)) ->
      let rnd = System.Random()
      M([for i in 1..rows -> [for j in 1..cols -> rnd.NextDouble()]])
  | _ -> M([])

(*************************************************)
let v0 = V([1.0..5.0])
printfn "v0 in R^%d =\n%s" (length v0) (VectorToString v0)

let v1 = -v0
printfn "-v0 = v1 in R^%d =\n%s" (length v1) (VectorToString v1)

let v2 = v0+V([0.0..0.1..0.4])
printfn "V0 + V([0.0..0.1..0.4]) = v2 in R^%d =\n%s" (length v2) (VectorToString v2)

let v3 = v0-V([0.0..0.1..0.4])
printfn "v0-V([0.0..0.1..0.4]) = v3 in R^%d =\n%s" (length v3) (VectorToString v3)

let v4 = 3.0*v3
printfn "3.0*v3 = v4 in R^%d =\n%s" (length v4) (VectorToString v4)

let v5 = v3*v4
printfn "v3*v4 = v5 = %f" v5

let v6 = VectorNorm v3
printfn "VectorNorm v3 = v6 = %f" v6

let n = length v0
let v7 = VectorGetSlice (v0, 0, n-1)
printfn "VectorGetSlice (v0, 0, (length v0) -1) = v7 = %s" (VectorToString v7)

let v8 = VectorGetSlice (v0, 0, 0)
printfn "VectorGetSlice (v0, 0, 0) = v8 = %s" (VectorToString v8)

let v9 = VectorGetSlice (v0, 1, 2)
printfn "VectorGetSlice (v0, 1, 2) = v9 = %s" (VectorToString v9)

printfn "VectorRandom 4 = %s"  (VectorToString (VectorRandom 4))

let m0 = M([[1.0; 2.0];[3.0;4.0]])
printfn "m0 in R^(%dx%d) =\n%s" (rows m0) (cols m0) (MatrixToString m0)

let m1 = 3.0*m0
printfn "3.0*m0 = m1 in R^(%dx%d) =\n%s" (rows m1) (cols m1) (MatrixToString m1)

let m2 = -m0
printfn "-m0 = m2 in R^(%dx%d) =\n%s" (rows m2) (cols m2) (MatrixToString m2)

let m3 = m0+M([[5.0; 6.0];[7.0;8.0]])
printfn "m0+M([[5.0; 6.0];[7.0;8.0]]) = m3 in R^(%dx%d) =\n%s" (rows m3) (cols m3) (MatrixToString m3)

let m4 = m0-M([[5.0; 6.0];[7.0;8.0]])
printfn "m0-M([[5.0; 6.0];[7.0;8.0]]) = m4 in R^(%dx%d) =\n%s" (rows m4) (cols m4) (MatrixToString m4)

let m5 = transpose m0
printfn "transpose m0 = m5 in R^(%dx%d) =\n%s" (rows m5) (cols m5) (MatrixToString m5)

let m6 = m0*m5
printfn "m0*m5 = m6 in R^(%dx%d) =\n%s" (rows m6) (cols m6) (MatrixToString m6)

let r = rows m0
let c = cols m0
printfn "MatrxGetSlice (m0, 0, r-1, 0, c-1) = %s" (MatrixToString (MatrixGetSlice (m0, 0, r-1, 0, c-1)))

printfn "MatrixGetSlice (m0, 0, 0, 0, 0)  = %s" (MatrixToString (MatrixGetSlice (m0, 0, 0, 0, 0)))

printfn "MatrixGetSlice (m0, 1, 1, 0, 1)  = %s" (MatrixToString (MatrixGetSlice (m0, 1, 1, 0, 1)))

printfn "RowConcat (m0, m1) = %s" (MatrixToString (RowConcat (m0, m1)))

printfn "ColConcat (m0, m1) = %s" (MatrixToString (ColConcat (m0, m1)))

printfn "Minor (m0, 0, 0) = %s" (MatrixToString (Minor (m0, 0, 0)))

printfn "Minor (m0, 1, 0) = %s" (MatrixToString (Minor (m0, 1, 0)))

printfn "Minor (m0, -1, 0) = %s" (MatrixToString (Minor (m0, -1, 0)))

printfn "Minor (m0, 0, 1) = %s" (MatrixToString (Minor (m0, 0, 1)))

printfn "Minor (m0, 0, -1) = %s" (MatrixToString (Minor (m0, 0, -1)))

printfn "Det m0 = %f" (Det m0)

let m7 = ColConcat (RowConcat (m0, m1), RowConcat (m2, m3))
printfn "m7 = ColConcat (RowConcat (m0, m1), RowConcat (m2, m3)) = %s" (MatrixToString m7)
printfn "Det m7 = %f" (Det m7)

let v10 = Cramer (m0,V([1.0; 2.0]))
printfn "v10 = Cramer (m0,V([1, 2])) = %s" (VectorToString v10)
printfn "VectorToMatrix v10 = %s" (MatrixToString (VectorToMatrix v10))
printfn "m0*v10 = %s" (MatrixToString (m0*v10))

printfn "VectorOne 3 = %s" (VectorToString (VectorOne 3))

let m8 = Kronecker (m0,m0)
printfn "m8 = Kronecker (m0,m0) = %s" (MatrixToString m8)

printfn "trace (m8) = %f" (trace m8)

printfn "id 4 = %s"  (MatrixToString (id 4))

printfn "MatrixRandom (4,3) = %s"  (MatrixToString (MatrixRandom (4, 3)))

printfn "Inverse m0 = %s"  (MatrixToString (Inverse m0))
printfn "(Inverse m0)*m0 = %s"  (MatrixToString ((Inverse m0)*m0))

let m = 8;
printf "Testing timing of generating and inverting a %dx%d random matrix:" m m
let stopWatch = System.Diagnostics.Stopwatch.StartNew()
let A = MatrixRandom (m,m)
let B = Inverse A
stopWatch.Stop()
printfn " %f ms" stopWatch.Elapsed.TotalMilliseconds
