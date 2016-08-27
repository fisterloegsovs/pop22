let translatePoint (d : float list) (p : float list) =
  [p.[0] + d.[0]; p.[1] + d.[1]]

let translateCurve d c =
  List.map (translatePoint d) c

let scalePoint s (p : float list) =
  [s * p.[0]; s * p.[1]]

let scaleCurve s c =
  List.map (scalePoint s) c

let rotatePoint theta (p : float list) =
  [p.[0] * cos theta + p.[1] * sin theta;
   - p.[0] * sin theta + p.[1] * cos theta]

let rotateCurve theta c =
  List.map (rotatePoint theta) c

let translateCurveTo target (c : float list list)=
  let source = scalePoint -1.0 c.[0]
  translateCurve target (translateCurve source c)

let center (c : float list list) =
  scalePoint (1.0/(float c.Length)) (List.fold translatePoint [0.0; 0.0] c)

let c = [
  [0.0; 0.0];
  [0.0; 1.0];
  [1.0; 1.0];
  [1.0; 0.0]]
let d = [0.5; 0.5]
let s = 0.1
let theta = 3.1415/2.0

printfn "Original curve\n%A" c
printfn "Curve translated %A\n%A" d (translateCurve d c)
printfn "Curve scaled %A\n %A" s (scaleCurve s c)
printfn "Curve rotated %A\n%A" theta (rotateCurve theta c)
