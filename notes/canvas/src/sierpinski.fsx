//
// Draw the Sierpinsky triangle fractal (https://en.wikipedia.org/wiki/Sierpi%C5%84ski_triangle)
//
#r "nuget:DIKU.Canvas, 1.0"

open Canvas

let rec triangle C len (x,y) =
  if len < 20.0 then
    setBox C blue (int(round x),int(round y))
                (int(round(x+len)),
                 int(round(y+len)))
  else let half = len / 2.0
       do triangle C half (x+half/2.0,y)
       do triangle C half (x,y+half)
       do triangle C half (x+half,y+half)

do runSimpleApp "Sierpinski" 600 600
      (fun w h -> let C = create w h
                  do triangle C (float(min w h)) (0.0,0.0);
                  C)
