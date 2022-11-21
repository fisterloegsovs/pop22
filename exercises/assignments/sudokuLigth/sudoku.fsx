module Sudoku

let board : int option [,] = Array2D.create 9 9 None

let flatten (arr: 'a[,]): 'a[] =
  let mutable vec = [||]
  for i = 0 to (Array2D.length2 arr - 1) do
    vec <- Array.append vec arr[*,i]
  vec

let get (i: int) (j: int) : int option =
  board[i,j]

let legal (i: int) (j: int) (v: int) : bool =
  let m = i/3
  let n = j/3
  Array.concat [flatten board[3*m..3*m+2,3*n..3*n+2]; board[i,*]; board[*,j]] 
  |> Array.exists (fun elm -> elm = Some v)
  |> not

let set (i: int) (j: int) (v: int) : bool =
  let isLegal = legal i j v
  if isLegal then board[i,j] <- Some v
  isLegal

let getHelp (i: int) (j: int) : int[] =
  Array.filter (fun elm -> legal i j elm) [|0..9|]

let print (): unit =
  