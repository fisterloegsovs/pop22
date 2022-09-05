//
// Interactively draw the Sierpinsky triangle fractal to a user specified depth
// See https://en.wikipedia.org/wiki/Sierpi%C5%84ski_triangle for more on Sierpinsky's triangle
//
#r "nuget:DIKU.Canvas, 1.0"

open Canvas

let rec pow2 x = pown 2 x

let setBoxF C c (x1,y1) (x2,y2) =
  do setBox C c (int(x1),int(y1)) (int(x2),int(y2))

let rec triangle C T (w,h) (x,y) =
  if w <= T then setBoxF C red (x,y) (x+w,y+h) 
  else let (w_half,h_half) = (w/2.0, h/2.0)
       do triangle C T (w_half,h_half) (x+w_half/2.0,y)
       do triangle C T (w_half,h_half) (x,y+h_half)
       do triangle C T (w_half,h_half) (x+w_half,y+h_half)

type state = int

let margin = 30.0

let draw w h (s:state) =
  let C = create w h
  let T = float(512 * pow2 s / w)
  let (w,h) = (float(w)-2.0*margin,
               float(h)-2.0*margin)
  do triangle C T (w,h) (margin,margin)
  C

let react (s:state) (k:key) : state option =
    match getKey k with
        | Space ->
            do printfn "Pressed space!"
            None
        | LeftArrow ->
            do printfn "Pressed left-arrow!"
            None
        | RightArrow ->
            do printfn "Pressed right-arrow!"
            None
        | DownArrow -> Some (min 9 (s+1))
        | UpArrow -> Some (max 2 (s-1))
        | _ -> None

let initState = 5
let width = 600
let height = width

do runApp "Sierp" width height draw react initState
