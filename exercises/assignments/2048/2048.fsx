#r "nuget:DIKU.Canvas, 1.0.1"
open Canvas

type pos = int*int // A 2-dimensional vector
type value = Red | Green | Blue | Yellow | Black
type piece = value*pos
type state = piece list // the board is a set of randomly organized pieces
let N = 3 // NxN is the size of the board
let w = 600 // The canvas will be wxw pixels

/// <summary>Convert a value type to a color</summary>
/// <param v>a (color-)value</param>
/// <returns>a canvas color</returns>
let fromValue (v:value) : color =
  match v with
    Red -> red
    | Green -> green
    | Blue -> blue
    | Yellow -> yellow
    | Black -> black

/// <summary>Cycle to the next color in the order red->green->blue->black->black->...</summary>
/// <param v>a (color-)value</param>
/// <returns>the next color-value</returns>
let nextColor (c: value) : value =
  match c with
    Red -> Green
    | Green -> Blue
    | Blue -> Yellow
    | _ -> Black

/// <summary>Find pieces on a particular column</summary>
/// <param i>a row number</param>
/// <param s>a list of pieces</param>
/// <returns>a sublist of pieces</returns>
let filter (k: int) (s: state): state = List.filter (fun (c,(m,n)) -> n = k) s

/// <summary>Shift all pieces up</summary>
/// <param s>a list of pieces</param>
/// <returns>same list of pieces but shifted</returns>
let shiftUp (s: state) : state =
  let rec collapse (s: state) : state =
    match s with
      a::b::rst when fst a = fst b -> (nextColor (fst b), snd b)::collapse rst
      | a::rst -> a::collapse rst
      | [] -> []
  let shift i =
    s
    |> filter i 
    |> List.sortBy (fun (c,(m,n))->m) 
    |> collapse
    |> List.mapi (fun j (c,(m,n))->(c,(j,n)))
  List.map shift [0..(N-1)] |> List.concat

/// <summary>Flip the board up-down</summary>
/// <param s>a list of pieces</param>
/// <returns>same list of pieces but flipped</returns>
let flipUD (s: state) : state =
  List.map (fun (c,(m,n)) -> (c,(N-1-m,n))) s

/// <summary>Transpose the board so rows becomes columns and vice versa</summary>
/// <param s>a list of pieces</param>
/// <returns>same list of pieces but transposed</returns>
let transpose (s: state) : state =
  List.map (fun (c,(m,n)) -> (c,(n,m))) s

/// <summary>Find empty positions on the board</summary>
/// <param s>a list of pieces</param>
/// <returns>a list of positions, with no pieces</returns>
let empty (s: state) : pos list =
  let all = List.allPairs [0..(N-1)] [0..(N-1)]
  let occupied = List.map (fun (c,p) -> p) s
  List.except occupied all

let rnd = System.Random()
/// <summary>Add a piece to a randomly selected empty position</summary>
/// <param c>a color-value</param>
/// <param s>a list of pieces</param>
/// <returns>a list of positions with one extra piece</returns>
let addRandom (c: value) (s: state) : state option =
  let available = empty s
  if available.Length = 0 then 
    None
  else
    let pos = available[rnd.Next (available.Length - 1)]
    Some ((c,pos)::s)
  
/// <summary>Create a canvas of the board</summary>
/// <param w>the width of the resulting canvas</param>
/// <param h>the height of the resulting canvas</param>
/// <param s>the board</param>
/// <returns>a canvas with pieces added</returns>
let draw (w: int) (h: int) (s:state) =
  let rec helper (C: canvas) (lst: state) : unit =
    match lst with 
      [] -> ()
      | (c,(i,j))::rst ->
        setFillBox C (fromValue c) (i*w/N,j*h/N) ((i+1)*w/N-1,(j+1)*w/N-1)
        helper C rst
  let C = create w h
  helper C s
  C

/// <summary>Convert a key-input to an offset</summary>
/// <param s>the list of pieces</param>
/// <param k>the pressed key</param>
/// <returns>an updated set of pieces</returns>
let react (s:state) (k:Canvas.key) : state option =
  match getKey k with
    | LeftArrow -> 
      s |> shiftUp |> addRandom Red 
    | RightArrow ->
      s |> flipUD |> shiftUp |> flipUD |> addRandom Red 
    | UpArrow ->
      s |> transpose |> shiftUp |> transpose  |> addRandom Red
    | DownArrow ->
      s |> transpose |> flipUD |> shiftUp |> flipUD |> transpose |> addRandom Red
    | _ -> None

// Initial pieces on an NxN board. positions must be in the range ([0..(N-1)],[0..(N-1)])
let board = [(Blue,(1,0));(Red,(0,0))]

// Enter the interactive canvas loop
do runApp "2048" w w draw react board
