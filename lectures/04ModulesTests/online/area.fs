module area

let areaCircle r = System.Math.PI * r * r
let areaAnnulus R r = areaCircle R - areaCircle r
