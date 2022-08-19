// 2020-21; 5g0

// a)
let isTable (t : 'a list list) : bool =
  if List.isEmpty t then false
  else let sz = List.length (List.head t)
       in List.fold (fun a x -> a && sz = List.length x) true t

// Helper function
let columns (t : 'a list list) : int =
  if isTable t then List.length(List.head t)
  else 0

// b)
let firstColumn (t : 'a list list) : 'a list =
  if columns t > 0 then
    List.map List.head t
  else []

// c)
let dropFirstColumn (t : 'a list list) : 'a list list =
  if columns t > 0 then
    List.map List.tail t
  else []

// d)
let transposeLstLst (t : 'a list list) : 'a list list =
  let mutable t1 = t
  let mutable t2 = []
  do while (columns t1 > 0) do
       t2 <- t2 @ [firstColumn t1]
       t1 <- dropFirstColumn t1
  t2

let t1 : int list list = [[1;2];[3;4];[5;6]]
let t1T : int list list = [[1;3;5];[2;4;6]]

// Test of a)
do printf "test a1: %A\n" (isTable t1 = true)
do printf "test a2: %A\n" (isTable [[1;2];[3]] = false)
do printf "test a3: %A\n" (isTable [[]] = true)
do printf "test a4: %A\n" (isTable [] = false)

// Test of b)
do printf "test b1: %A\n" (firstColumn [] = [])
do printf "test b2: %A\n" (firstColumn t1 = [1;3;5])
do printf "test b3: %A\n" (firstColumn [[1;2];[3]] = [])

// Test of c)
do printf "test c1: %A\n" (dropFirstColumn [] = [])
do printf "test c2: %A\n" (dropFirstColumn t1 = [[2];[4];[6]])
do printf "test c3: %A\n" (dropFirstColumn [[1;2];[3]] = [])

// Test of d)
do printf "test d1: %A\n" (transposeLstLst [[1]] = [[1]])
do printf "test d2: %A\n" (transposeLstLst t1 = t1T)
do printf "test d3: %A\n" (transposeLstLst t1T = t1)
do printf "test d4: %A\n" (transposeLstLst [[1;2];[3]] = [])
do printf "test d5: %A\n" (transposeLstLst [[];[]] = [])      // uggh
do printf "test d6: %A\n" (transposeLstLst [[];[];[]] = [])   // uggh

//
// We do not have the property
//
//   isTable t => transposeLstLst (transposeLstLst t) = t
//
// A way to fix this problem is to redefine the specification of
// isTable so that no dimension is allowed to be empty.
//
