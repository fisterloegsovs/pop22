let testEqual str a b =
  printfn "%5b: %s" (a = b) str

// Testing empty parameter list, behaviour undefined
testEqual "polynomial [] 3.0" (Collection.polynomial [] 3.0) (0.0)

// Testing 1st order polynomial
testEqual "polynomial [1.0] 3.0" (Collection.polynomial [1.0] 3.0) (1.0 * 3.0 ** 0.0)
testEqual "polynomial [0.0] 3.0" (Collection.polynomial [0.0] 3.0) (0.0 * 3.0 ** 0.0)
testEqual "polynomial [-1.0] 3.0" (Collection.polynomial [-1.0] 3.0) (-1.0 * 3.0 ** 0.0)
testEqual "polynomial [1.0] 0.0" (Collection.polynomial [1.0] 0.0) (1.0 * 0.0 ** 0.0)
testEqual "polynomial [1.0] -3.0" (Collection.polynomial [1.0] -3.0) (1.0 * -3.0 ** 0.0)

// Testing 2nd order polynomial
testEqual "polynomial [1.0; 2.0] 3.0" (Collection.polynomial [1.0; 2.0] 3.0) (1.0 * 3.0 ** 0.0 + 2.0 * 3.0 ** 1.0)
testEqual "polynomial [0.0; 2.0] 3.0" (Collection.polynomial [0.0; 2.0] 3.0) (0.0 * 3.0 ** 0.0 + 2.0 * 3.0 ** 1.0)
testEqual "polynomial [-1.0; 2.0] 3.0" (Collection.polynomial [-1.0; 2.0] 3.0) (-1.0 * 3.0 ** 0.0 + 2.0 * 3.0 ** 1.0)
testEqual "polynomial [1.0; 2.0] 0.0" (Collection.polynomial [1.0; 2.0] 0.0) (1.0 * 0.0 ** 0.0 + 2.0 * 0.0 ** 1.0)
testEqual "polynomial [1.0; 2.0] -3.0" (Collection.polynomial [1.0; 2.0] -3.0) (1.0 * -3.0 ** 0.0 + 2.0 * -3.0 ** 1.0)

testEqual "polynomial [1.0; 0.0] 3.0" (Collection.polynomial [1.0; 0.0] 3.0) (1.0 * 3.0 ** 0.0 + 0.0 * 3.0 ** 1.0)
testEqual "polynomial [0.0; 0.0] 3.0" (Collection.polynomial [0.0; 0.0] 3.0) (0.0 * 3.0 ** 0.0 + 0.0 * 3.0 ** 1.0)
testEqual "polynomial [-1.0; 0.0] 3.0" (Collection.polynomial [-1.0; 0.0] 3.0) (-1.0 * 3.0 ** 0.0 + 0.0 * 3.0 ** 1.0)
testEqual "polynomial [1.0; 0.0] 0.0" (Collection.polynomial [1.0; 0.0] 0.0) (1.0 * 0.0 ** 0.0 + 0.0 * 0.0 ** 1.0)
testEqual "polynomial [1.0; 0.0] -3.0" (Collection.polynomial [1.0; 0.0] -3.0) (1.0 * -3.0 ** 0.0 + 0.0 * -3.0 ** 1.0)

testEqual "polynomial [1.0; -2.0] 3.0" (Collection.polynomial [1.0; -2.0] 3.0) (1.0 * 3.0 ** 0.0 + -2.0 * 3.0 ** 1.0)
testEqual "polynomial [0.0; -2.0] 3.0" (Collection.polynomial [0.0; -2.0] 3.0) (0.0 * 3.0 ** 0.0 + -2.0 * 3.0 ** 1.0)
testEqual "polynomial [-1.0; -2.0] 3.0" (Collection.polynomial [-1.0; -2.0] 3.0) (-1.0 * 3.0 ** 0.0 + -2.0 * 3.0 ** 1.0)
testEqual "polynomial [1.0; -2.0] 0.0" (Collection.polynomial [1.0; -2.0] 0.0) (1.0 * 0.0 ** 0.0 + -2.0 * 0.0 ** 1.0)
testEqual "polynomial [1.0; -2.0] -3.0" (Collection.polynomial [1.0; -2.0] -3.0) (1.0 * -3.0 ** 0.0 + -2.0 * -3.0 ** 1.0)

// Testing a 3rd order polynomial
testEqual "polynomial [1.0; 2.0; 1.0] 3.0" (Collection.polynomial [1.0; 2.0; 1.0] 3.0) (1.0 * 3.0 ** 0.0 + 2.0 * 3.0 ** 1.0 + 1.0 * 3.0 ** 2.0)

// Test a 7th order polynomial
testEqual "polynomial [1.0; -2.0; 3.0; -4.0; 0.1; 0.2; 0.3] -3.0" (Collection.polynomial [1.0; -2.0; 3.0; -4.0; 0.1; 0.2; 0.3] -3.0) (1.0 * -3.0 ** 0.0 + -2.0 * -3.0 ** 1.0 + 3.0 * -3.0 ** 2.0 + -4.0 * -3.0 ** 3.0 + 0.1 * -3.0 ** 4.0 + 0.2 * -3.0 ** 5.0 + 0.3 * -3.0 ** 6.0)
