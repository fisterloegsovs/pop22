let testEqual str a b =
  printfn "%5b: %s" (a = b) str

// Testing empty parameter list, behaviour undefined
testEqual "listToString [3.0; 4.0]" (Collection.listToString [3.0; 4.0]) "(3.0, 4.0)"

printfn "\nDone"
