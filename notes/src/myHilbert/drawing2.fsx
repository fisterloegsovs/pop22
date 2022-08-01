open Curve

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
