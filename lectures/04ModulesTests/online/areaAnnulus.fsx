let areaCircle r = 
  System.Math.PI * r * r

let areaAnnulus R r =
  areaCircle R - areaCircle r

printfn "%.10f" (areaAnnulus 3.5 1.5)

