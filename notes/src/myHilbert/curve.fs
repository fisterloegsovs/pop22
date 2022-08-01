module Curve

let translatePoint (d : float list) (p : float list) =
  [p.[0] + d.[0]; p.[1] + d.[1]]

let translateCurve d c =
  List.map (translatePoint d) c

let scalePoint s (p : float list) =
  [s * p.[0]; s * p.[1]]

let scaleCurve s c =
  List.map (scalePoint s) c

let rotatePoint theta (p : float list) =
  [p.[0] * cos theta - p.[1] * sin theta;
   + p.[0] * sin theta + p.[1] * cos theta]

let rotateCurve theta c =
  List.map (rotatePoint theta) c

let translateCurveTo target (c : float list list)=
  let source = scalePoint -1.0 c.[0]
  translateCurve target (translateCurve source c)

let turnLeft (dir, c) = (dir + 3.141592/2.0, c)

let turnRight (dir, c) = (dir - 3.141592/2.0, c)

let draw (dir, (c : float list list)) =
  let nextPoint = rotatePoint dir [1.0; 0.0]
  (dir, c @ [translatePoint c.[c.Length-1] nextPoint])
  
let minPoint (p1 : float list) (p2 : float list) =
  [min p1.[0] p2.[0]; min p1.[1] p2.[1]]      
    
let maxPoint (p1 : float list) (p2 : float list) =
  [max p1.[0] p2.[0]; max p1.[1] p2.[1]]      
    
let minimum c =
  List.fold minPoint [infinity; infinity] c    

let maximum c =
  List.fold maxPoint [-infinity; -infinity] c    

let center (c : float list list) =
  scalePoint (1.0/(float c.Length)) (List.fold translatePoint [0.0; 0.0] c)
