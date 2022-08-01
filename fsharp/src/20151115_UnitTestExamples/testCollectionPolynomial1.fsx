let yTest = Collection.polynomial [1.0; 2.0; 1.0] 3.0 // = 1.0 * 3.0 ** 0.0 + 2.0 * 3.0 ** 1.0 + 1.0 * 3.0 ** 2.0 = 16.0
let yTarget = 1.0 * 3.0 ** 0.0 + 2.0 * 3.0 ** 1.0 + 1.0 * 3.0 ** 2.0;

printfn "%f" yTest
printfn "(%f, %f)" yTest yTarget
printfn "%5b: polynomial [1.0; 2.0; 1.0] 3.0" (yTest = yTarget)
printfn "%5b: polynomial [1.0; 2.0; 1.0] 3.0" (yTest = 15.0)

let testEqual str a b =
  printfn "%5b: %s" (a = b) str

testEqual "polynomial [1.0; 2.0; 1.0] 3.0" yTarget yTest
testEqual "polynomial [1.0; 2.0; 1.0] 3.0" (Collection.polynomial [1.0; 2.0; 1.0] 3.0) (1.0 * 3.0 ** 0.0 + 2.0 * 3.0 ** 1.0 + 1.0 * 3.0 ** 2.0)
