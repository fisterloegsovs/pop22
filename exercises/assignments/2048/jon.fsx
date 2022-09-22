#r "nuget:DIKU.Canvas, 1.0"
open Canvas

type pos = int*int // A 2-dimensional vector
type value = Red | Green | Blue | Black
type piece = value*pos
type state = piece list
let N = 4

let fromValue (v:value) : color =
  match v with
    Red -> red
    | Green -> green
    | Blue -> blue
    | Black -> black

let nextColor (c: value) : value =
  match c with
    Red -> Green
    | Green -> Blue
    | _ -> Black

let filter s r = List.filter (fun (c,(m,n)) -> n = r) s

/// <summary>Create a canvas with 36 spokes, centered in the middle, and with an inital angular offset</summary>
/// <param w>the width of the resulting canvas</param>
/// <param h>the height of the resulting canvas</param>
/// <param s>the angular offset of all spokesr</param>
/// <returns>a canvas with spokes</returns>
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

let shiftLeft (s: state) : state =
  let rec collapse (s: state) : state =
    match s with
      a::b::rst when fst a = fst b -> (nextColor (fst b), snd b)::collapse rst
      | a::rst -> a::collapse rst
      | [] -> []
  let shift i =
    filter s i 
    |> List.sortBy (fun (c,(m,n))->m) 
    |> collapse
    |> List.mapi (fun j (c,(m,n))->(c,(j,n)))
  List.map shift [0..3] |> List.concat

let fliplr (s: state) : state =
  List.map (fun (c,(m,n)) -> (c,(N-1-m,n))) s

let transpose (s: state) : state =
  List.map (fun (c,(m,n)) -> (c,(n,m))) s

let empty (s: state) : pos list =
  let all = List.allPairs [0..3] [0..3]
  let full = List.map (fun (c,p) -> p) s
  List.except full all

let rnd = System.Random()
let addRandom (c: value) (s: state) : state =
  let available = empty s
  let pos = available[rnd.Next (available.Length - 1)]
  (c,pos)::s

/// <summary>Convert a key-input to an offset</summary>
/// <param s>the present angular offset of all spokesr</param>
/// <param k>the pressed key</param>
/// <returns>an updated offset</returns>
let react (s:state) (k:Canvas.key) : state option =
  match getKey k with
    | LeftArrow -> 
      s |> shiftLeft |> addRandom Red |> Some
    | RightArrow ->
      s |> fliplr |> shiftLeft |> fliplr |> addRandom Red |> Some
    | UpArrow ->
      s |> transpose |> shiftLeft |> transpose  |> addRandom Red|> Some
    | DownArrow ->
      s |> transpose |> fliplr |> shiftLeft |> fliplr |> transpose |> addRandom Red |> Some
    | _ -> None

let board = [(Blue,(1,0));(Red,(0,0));(Green,(3,2))]

// Enter the interactive canvas loop
do runApp "ColorTest" 600 600 draw react board
