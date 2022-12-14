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
      
/// Generate a histogram of the characters 'a'..'z' in a given string.
let histogram (str : string) : int list =
  /// Count the number of occurrences of character c in the string str.
  let rec countChars (str : string) (c : char) : int =
    if str.Length = 0 then
      0
    else
      System.Convert.ToInt32 (str.[0] = c) + countChars str.[1..] c

  /// Generate the histogram of characters c..'z' in the string str.
  let rec appendHistVal (str : string) (c : char) : int list =
    if c > 'z' then
      []
    else
      countChars str c :: appendHistVal str (char (int c + 1))

  appendHistVal str 'a'

////////////////////////////////////
// Given a string, calculate its histogram, generate a new random string with a similar histogram, and compare the two.
//let str = "abcdefghijklmnopqrstuvxyz"
let str = "abcz"
printfn "A string: %s" str
let hist = histogram str
let alphabet = List.init hist.Length (fun i -> 'a' + char i)
printfn "A histogram:\n %A" (List.zip alphabet hist)

//let cumHist = cumSum hist
//printfn "the cumulated histogram:\n %A" (List.zip alphabet cumHist)
//let revInd = [0..cumHist.[cumHist.Length-1]]
//printfn "Reverse lookup:\n %A" (List.zip revInd (List.map (reverseLookup cumHist) revInd))

let ranStr = randomString hist 1000
printfn "A random string: %s" ranStr
let newHist = histogram ranStr
printfn "Resulting histogram:\n %A" (List.zip alphabet newHist)
