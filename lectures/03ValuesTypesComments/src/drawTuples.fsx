#r "nuget:diku.canvas, 1.0.1"
open Canvas

let  circ (C: canvas) (col: color) (r: float) (c: float*float) (n: int) : unit = 
  let rec arc (da: float) (i: int) : unit =
    match i with
      0 -> ()
      | _ ->
        let a = da*float i
        let b = da*float (1+i)
        let p1 = (int (x+r*cos a), int (y+r*sin a))
        let p2 = (int (x+r*cos b), int (y+r*sin b))
        setLine C col p1 p2
        arc da (i-1)
  arc (2.0*System.Math.PI/float n) n

let w = 800
let h = w
let C = create w h  
let center = ((float w)/2.0, (float w)/2.0)
let radius = (float w)*3.0/8.0
circ C black radius center 36
show C "Circle"
