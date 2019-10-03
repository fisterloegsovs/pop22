/// Reverse the order of a list of any type
let rec reverse (lst : 'a list) : 'a list =
  match lst with
    elm :: rest -> (reverse rest) @ [elm]
    | [] -> []

/// Calculate the cumulative sum of a list of integers from the first to the last element. First element is the first number in the original list, last element is the sum of all integers in the original list.
let cumSum (lst : int list) : int list =
  /// Prepend the sum of the first element in acc and elm to acc.
  let updateCumSum (acc : int list) (elm : int) : int list =
    match acc with
      [] -> [elm]
      | a -> a.Head + elm :: a

  List.fold updateCumSum [] lst |> reverse

/// Given a monotonic function and an index into its value set, find the corresponding value on its definition set.
let reverseLookup (monotonic : 'a list) (v : 'a) : int =
    try
      List.findIndex (fun w -> w > v) monotonic
    with
      _ -> monotonic.Length - 1

/// Generate a random character according to a histogram.
let rnd = System.Random() // A global object, if included in randomChar then seed point only changes slowly giving strongly correlated results in time
let randomChar (hist : int list) : char =
  let cumHist = cumSum hist
  let v = rnd.Next(cumHist.[cumHist.Length-1])
  let i = reverseLookup cumHist v
  'a' + char i

/// Generate a string of random characters each distributed according to a histogram.
let randomString (hist : int list) (len : int) : string = 
  /// Append random characters to a string, each distributed according to a histogram.
  let rec appendRandomString (hist : int list) (len : int) (src : string) : string = 
    if len = 0 then
      src
    else
      src + string (randomChar hist) |> appendRandomString hist (len-1)

  appendRandomString hist len ""
      
//////////////////////////////////////
/// Generate a histogram of the characters 'a'..'z' in a given string.
let histogram (str : string) : int list =
  /// Count the number of occurrences of character c in the string str.
  let rec countChars (str : string) (c : char) : int =
    if str.Length = 0 then
      0
    else
      System.Convert.ToInt32 (str.[0] = c) + countChars str.[1..] c

  /// Generate the histogram of characters c..'z' in the string str.
  List.map (countChars str) ['a'..'z']
  
let readfile filename = 
  try
    let reader = System.IO.File.OpenText filename
    reader.ReadToEnd ()
  with
    _ -> "" // The file cannot be read, so we return an empty string

let rec stringToList (str : string) =
  if str.Length > 0 then
    str.[0] :: stringToList str.[1..]
  else
    []

let listToString cList =
  List.fold (fun acc c -> acc + (string c)) "" cList

let filter (str : string) : string =
  str.ToLower () |> String.filter (fun c -> c >= 'a' && c <='z')      

let cooccurrence (str : string) : int list list = 
  let lst = stringToList str
  let rec neighbourOf (lst : char list) (c : char) : char list =
    match lst with
      a::b::rst when a = c -> b::(neighbourOf (b::rst) c)
      | a::rst -> neighbourOf rst c
      | _ -> []
      
  let neighbourOfHist (lst : char list) (c : char) : int list =
    neighbourOf lst c |> listToString |> histogram

  List.map (neighbourOfHist lst) ['a'..'z']

let fstOrderMarkovModel (cooc:int list list) (len: int) : string =
  let rec appendRandomString (coo : int list list) (len : int) (last : char) : string = 
    if len > 0 then
      let hist = coo.[int last - int 'a']
      let next = randomChar hist
      string next + (appendRandomString coo (len - 1) next)
    else
      ""
  appendRandomString cooc len 'a'

let compareHist (left : int list) (right : int list) : int =
  List.zip left right |> List.fold (fun acc (l,r) -> acc + (abs (l - r))) 0

let compareCooccurrence (left : int list list) (right : int list list) : int =
  List.zip left right |> List.fold (fun acc (l,r) -> acc + compareHist l r) 0
  
////////////////////////////////////
// Given a string, calculate its histogram, generate a new random string with a similar histogram, and compare the two.
//let filename = "readFile.fsx"
let filename = "littleClausAndBigClaus.txt"
let str = readfile filename
//printfn "%s\n" str

let strFiltered = filter str
printfn "A string:\n%s\n" strFiltered

let hist = histogram strFiltered
let alphabet = List.init hist.Length (fun i -> 'a' + char i)
printfn "A histogram:\n%A\n" (List.zip alphabet hist)

let ranStr = randomString hist strFiltered.Length
//let ranStr = randomString (List.init 26 (fun _ -> 1)) strFiltered.Length
printfn "A random string:\n%s\n" ranStr
let newHist = histogram ranStr
printfn "Resulting histogram:\n%A\n" (List.zip alphabet newHist)
let histDist = compareHist hist newHist
printfn "Absolute difference: %d\n" histDist

let coo = cooccurrence strFiltered
printfn "A cooccurrence table:\n%A\n" (List.zip alphabet coo)

let fstOrderStr = fstOrderMarkovModel coo strFiltered.Length
printfn "A random string:\n%s\n" fstOrderStr
let newCoo = cooccurrence  fstOrderStr
printfn "Resulting cooccurrence table:\n%A\n" (List.zip alphabet newCoo)
let cooDist = compareCooccurrence coo newCoo
printfn "Absolute difference: %d\n" cooDist

