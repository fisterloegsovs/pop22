module area

let areaCircle r = System.Math.PI * r * r

// We assume that R > r.
// Note to self: add error handling in the future
let areaAnnulus R r = areaCircle R - areaCircle r
