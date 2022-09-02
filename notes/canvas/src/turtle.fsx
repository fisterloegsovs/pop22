//
// Simple turtle graphics
//
#r "nuget:DIKU.Canvas, 1.0.0-alpha2"

open Canvas

let rec repeat n cmds =
  if n <= 0 then []
  else cmds @ repeat (n-1) cmds

let circ sz n =
  let x = 360 / n
  repeat n [Move sz; Turn x]

let star sz =
  repeat 5 [Move sz; Turn 144]

let rec tree sz =
   if sz < 5 then [Move sz; PenUp; Move (-sz); PenDown]
   else [Move (sz/3); 
         Turn -30] @
        tree (sz*2/3) @
        [Turn 30; 
         Move (sz/6); 
         Turn 25] @
        tree (sz/2) @
        [Turn -25; 
         Move (sz/3); 
         Turn 25] @
        tree (sz/2) @
        [Turn -25;
         Move (sz/6);
         PenUp;
         Move (-sz/3);
         Move (-sz/6);
         Move (-sz/3);
         Move (-sz/6);
         PenDown]

let pic = [SetColor red] @
          circ 10 12 @
          [SetColor green] @
          circ 10 24 @
          [SetColor blue] @
          circ 10 36 @
          [SetColor yellow] @
          circ 10 48 @
          [SetColor blue;
           PenUp;
           Turn -45;
           Move 200;
           PenDown] @
           circ 30 6 @
           [PenUp;
            Turn 120;
            Move 200;
            PenDown;
            SetColor red] @
           star 70 @
           [PenUp; Turn 130; Move 400; Turn 150; PenDown] @ 
           tree 200 @
           [PenUp;Turn 90; Move 100; Turn -85; PenDown] @ 
           tree 150

do turtleDraw (600,600) "Turtle" pic 
