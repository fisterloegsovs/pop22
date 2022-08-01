// Analyze a string for leading spaces. If empty or only spaces, then "" is returned
let rec skipInitial (str : string) =
  if (String.length str > 0) && (str.[0] = ' ') then
    if String.length str > 1 then
      skipInitial str.[1..]
    else
      ""
  else
    str

// Find first occurence of space and return index. If none, then index string length
let rec findFirst (str : string) j =
  if (j < String.length str) && (str.[j] <> ' ') then
    findFirst str (j + 1)
  else
    j
    
// Split a string and print each word separated by spaces
let rec split str =
  let rest = skipInitial str
  if String.length rest > 0 then
    let j = findFirst rest 0
    printfn "%A" rest.[..(j-1)]
    if j < String.length rest then
      split rest.[j..]
    else
      ()
  else
    ()

let test f str =
  printfn "Testing %A:" str
  f str

test split "hej jon"
test split "hej"
test split " hej jon"
test split "hej  jon"
test split "hej jon "
