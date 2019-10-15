/// <summary> Reverse the order of a list of any type </summary>
/// <param lst> Any list </param>
/// <returns> The list lst in reverse order </returns>
let rec reverse (lst : 'a list) : 'a list =
  match lst with
    elm :: rest -> (reverse rest) @ [elm]
    | [] -> []

/// <summary> Calculate the cumulative sum of a list of integers from
/// the first to the last element. First element is the first number
/// in the original list, last element is the sum of all integers in
/// the original list. </summary>
/// <param lst> A list </param>
/// <returns> A cumulative summed list. E.g., for lst = [e1; e2; e3]
/// then [e1; e1+e2; e1+e2+e3] is returned. </returns>
let cumSum (lst : int list) : int list =
  /// Prepend the sum of the first element in acc and elm to acc.
  let updateCumSum (acc : int list) (elm : int) : int list =
    match acc with
      [] -> [elm]
      | a -> a.Head + elm :: a

  List.fold updateCumSum [] lst |> reverse

/// <summary> Given a monotonic function and an index into its value
/// set, find the corresponding value on its definition
/// set. </summary>
/// <param monotonic> a list of samples of a monotonic increasing
/// function. E.g., if monotonic = [e1; e2; e3] then e1 <= e2 <= e3
/// </param>
/// <param v> A value in the codomain of monotonic </param>
/// <returns> A value in the domain of monotonic approximately
/// corresponding to v. E.g., if monotonic.[i] = v then reverseLookup
/// v = i </returns>
let reverseLookup (monotonic : 'a list) (v : 'a) : int =
    try
      List.findIndex (fun w -> w > v) monotonic
    with
      _ -> monotonic.Length - 1

/// <summary> Generate a random character according to a
/// histogram. </summary>
/// <param hist> A list of histogram values with count hist.[0] being
/// the value for 'a', hist.[1] for 'b' etc. </param>
/// <returns> A character randomly drawn from a distribution
/// resembling hist. </returns>

let randomChar (hist : int list) : char =
  let cumHist = cumSum hist
  let rnd = System.Random()
  let v = rnd.Next(cumHist.[cumHist.Length-1])
  let i = reverseLookup cumHist v
  printf "%A\r" (v,i) // rnd.Next seems to have a bug, and repeated
                      // calls are very often identical to the
                      // previous. A printf statement is included to
                      // increase the frequency.
  'a' + char i

/// <summary> Generate a string of random characters each distributed
/// according to a histogram. </summary>
/// <param hist> A list of histogram values </param>
/// <param len> The length of the resulting string </param>
/// <returns> A string of lenth len whose values are independently
/// drawn from a distribution resembling hist </returns>
let randomString (hist : int list) (len : int) : string = 
  /// Append random characters to a string, each distributed according
  /// to a histogram.
  let rec appendRandomString (hist : int list) (len : int) (src : string) : string = 
    if len = 0 then
      src
    else
      src + string (randomChar hist) |> appendRandomString hist (len-1)

  appendRandomString hist len ""
      
/// <summary> Generate a histogram of the characters 'a'..'z' in a
/// given string. </summary>
/// <param str> Any string consisting of the characters: 'a'..'z' and
/// ' ' in any order. </param>
/// <returns> A list of character counts, with the first element is
/// the count of 'a's in str, second the count of 'b's etc. </returns>
let histogram (str : string) : int list =
  // *****************************************************************
  // Mockup code, replace with code for calculating the histrogram
  // from a string.
  List.init 26 (fun _ -> 1)
  // *****************************************************************
  
//////////////////////////////////////////////////////////////////////
// Given a string, calculate its histogram, generate a new random
// string with a similar histogram, and compare the two.
//let str = "abcdefghijklmnopqrstuvxyz"
let str = "abcz"
printfn "A string: %s" str
let hist = histogram str
let alphabet = List.init hist.Length (fun i -> 'a' + char i)
printfn "A histogram:\n %A" (List.zip alphabet hist)

let ranStr = randomString hist 1000
printfn "A random string: %s" ranStr
let newHist = histogram ranStr
printfn "Resulting histogram:\n %A" (List.zip alphabet newHist)
