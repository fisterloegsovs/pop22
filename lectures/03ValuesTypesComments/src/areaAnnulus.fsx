let areaCircle r =
  printfn "%A" (System.Math.PI * r * r)
let areaAnnulus R r = 
  printfn "%A" (areaCircle R - areaCircle r)
printfn "%A" (areaAnnulus 3.5 1.5)
