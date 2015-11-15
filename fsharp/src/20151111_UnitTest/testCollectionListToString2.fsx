let testEqual str a b =
  if (a = b) then
    printf "."
  else
    printfn "\n%s: Got %A, should have been %A" str a b

// Test empty list
testEqual "listToString []" (Collection.listToString []) "()"

// Test singleton list
testEqual "listToString ['a']" (Collection.listToString ['a']) "(a)"

// Test 2 elements list
testEqual "listToString [3.0; 4.0]" (Collection.listToString [3.0; 4.0]) "(3.0, 4.0)"
testEqual "listToString [3.1; 4.5]" (Collection.listToString [3.1; 4.5]) "(3.1, 4.5)"

// Test many elements list
testEqual "listToString [3.1; 4.5; 4.5; 1.3; 7.4545; 32000.3]" (Collection.listToString [3.1; 4.5; 4.5; 1.3; 7.4545; 32000.3]) "(3.1, 4.5, 4.5, 1.3, 7.4545, 32000.3)"

// Test non-empty mixed list
testEqual "listToString ['a'; 4.5]" (Collection.listToString ['a'; 4.5]) "(a, 4.5)"
testEqual "listToString [true; 4.5]" (Collection.listToString [true; 4.5]) "(True, 4.5)"
testEqual "listToString [\"asdf\"; 4.5]" (Collection.listToString ["asdf"; 4.5]) "(asdf, 4.5)"

printfn "\nDone"
