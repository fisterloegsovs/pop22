// Split a string and print each word separated by spaces
let split str =
  let mutable i = 0
  for j = 0 to String.length str - 1 do
    if str.[j] = ' ' then
      printfn "%A" str.[i..(j-1)]
      i <- j + 1
  if i < String.length str - 1 then
    printfn "%A" str.[i..]

let testSplit str =
  printfn "Testing %A:" str
  split str

testSplit "hej jon"
testSplit "hej"
testSplit " hej jon"
testSplit "hej  jon"
testSplit "hej jon "
