let areaCircle r = 
  printfn "%g" (System.Math.PI * r * r)
let areaAnnulus R r = 
  printfn "%g" (areaCircle R - areaCircle r)
printfn "%g" (areaAnnulus 3.5 1.5)
