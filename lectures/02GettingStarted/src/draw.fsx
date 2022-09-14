#r "nuget:diku.canvas, 1.0.1"
open Canvas

let  circ (C: canvas) (col: color) (r: float) (x: float) (y: float) (n: int) : unit = 
  let rec arc (da: float) (i: int) : unit =
    match i with
      0 -> ()
      | _ ->
        let a = da*float i
        let b = da*float (1+i)
        let x1 = int (x+r*cos a)
        let y1 = int (y+r*sin a)
        let x2 = int (x+r*cos b)
        let y2 = int (y+r*sin b)
        setLine C col (x1,y1) (x2,y2)
        arc da (i-1)
  arc (2.0*System.Math.PI/float n) n
    
let w = 800
let h = w
let C = create w h  
let half = (float w)/2.0
let quarter = (float w)*3.0/8.0
circ C black quarter half half 36
show C "Circle"
