// Analyze a string for leading spaces. If empty or only spaces, then "" is returned
let skipInitial (str : string) =
  let mutable rest = str
  while (String.length rest > 0) && (rest.[0] = ' ') do
    if String.length rest > 1 then
      rest <- rest.[1..]
    else
      rest <- ""
  rest

// Find first occurence of space and return index. If none, then index string length
let findFirst (str : string) =
  let mutable j = 0
  while (j < String.length str) && (str.[j] <> ' ') do
    j <- j + 1
  j
    
// Split a string and print each word separated by spaces
let split str =
  let mutable rest = skipInitial str
  while String.length rest > 0 do
    let j = findFirst rest
    printfn "%A" rest.[..(j-1)]
    if j < String.length rest then
      rest <- skipInitial rest.[j..]
    else
      rest <- ""
  ()
      
let test f str =
  printfn "Testing %A:" str
  f str

test split "hej jon"
test split "hej"
test split " hej jon"
test split "hej  jon"
test split "hej jon "
