module textAnalysis

/// <summary> Calculate the cumulative sum of a list of integers from the first
/// to the last element. First element is the first number in the original list,
/// last element is the sum of all integers in the original list. </summary>
/// <param name = "lst"> A list </param>
/// <returns> A cumulative summed list. E.g., for lst = [e1; e2; e3],
/// [e1; e1+e2; e1+e2+e3] is returned. </returns>
let cumSum (lst : int list) : int list =
  List.tail (List.scan (+) 0 lst)

/// <summary> Given a monotonic function and an index into its value set, find
/// the corresponding value on its definition set. </summary>
/// <param name = "monotonic"> A list of samples of a monotonically increasing
/// function. E.g., if monotonic = [e1; e2; e3] then e1 <= e2 <= e3 </param>
/// <param name = "v"> A value in the codomain of monotonic </param>
/// <returns> A value in the domain of monotonic approximately corresponding to
/// v. E.g., if monotonic.[i] = v then reverseLookup v = i </returns>
let reverseLookup (monotonic : 'a list) (v : 'a) : int =
    // `findIndex` will throw an exception if `v` is larger than all elements in
    // `monotonic`. The try-with expression will default to the with-clause if
    // such an exception is thrown.
    try
      List.findIndex (fun w -> w > v) monotonic
    with
      _ -> monotonic.Length - 1

// The random generator is created outside any function call. This is done to
// use a single seed for all random numbers, thus avoiding using the same seed
// more than once.
let rnd = System.Random()

// The remaining functions are specialized to work with the following alphabet
let alphabet = ['a'..'z']@[' ']

/// <summary> Generate a random character according to a histogram. </summary>
/// <param name = "hist"> A list of histogram values with count hist.[0] being
/// the value for 'a', hist.[1] for 'b' etc. </param>
/// <returns> A character randomly drawn from a distribution resembling
/// hist. </returns>
let randomChar (hist : int list) : char =
  let cumHist = cumSum hist
  let v = rnd.Next(cumHist.[cumHist.Length-1])
  let i = reverseLookup cumHist v
  alphabet.[i] // Warning, this may cause an index out-of-bound exception

/// <summary> Generate a string of random characters each distributed according
/// to a histogram. </summary>
/// <param name = "hist"> A list of histogram values </param>
/// <param name = "len"> The length of the resulting string </param>
/// <returns> A string of lenth len whose values are independently drawn from a
/// distribution resembling hist </returns>
let randomString (hist : int list) (len : int) : string =
  String.init len (fun _ -> string (randomChar hist))

/// <summary> Generate a histogram of the characters 'a'..'z' in a given
/// string. </summary>
/// <param name = "str"> Any string consisting of the characters: 'a'..'z' and
/// ' ' in any order. </param>
/// <returns> A list of character counts, with the first element is the count of
/// 'a's in str, second the count of 'b's etc. </returns>
let histogram (str : string) : int list =
  /// Count the number of occurrences of character c in the string str.
  let rec countChars (str : string) (c : char) : int =
    if str.Length = 0 then
      0
    else
      System.Convert.ToInt32 (str.[0] = c) + countChars str.[1..] c

  /// Generate the histogram of characters c..'z' in the string str.
  List.map (countChars str) alphabet
  
let readfile filename = 
  try
    let reader = System.IO.File.OpenText filename
    reader.ReadToEnd ()
  with
    _ -> "" // The file cannot be read, so we return an empty string

let convertText (str : string) : string =
  str.ToLower () |> String.filter (fun c -> List.contains c alphabet)      

let listToString cList =
  List.fold (fun acc c -> acc + (string c)) "" cList

let stringToList (str : string) =
  str.ToCharArray () |> Array.toList

let cooccurrence (str : string) : int list list = 
  let lst = stringToList str
  let rec neighbourOf (lst : char list) (c : char) : char list =
    match lst with
      a::b::rst when a = c -> b::(neighbourOf (b::rst) c)
      | a::rst -> neighbourOf rst c
      | _ -> []
      
  let neighbourOfHist (lst : char list) (c : char) : int list =
    neighbourOf lst c |> listToString |> histogram

  List.map (neighbourOfHist lst) alphabet

let char2ind (c : char) : int =
  try
    List.findIndex (fun elm -> elm = c) alphabet
  with
    _ -> -1

let fstOrderMarkovModel (cooc:int list list) (len: int) : string =
  let rec appendRandomString (coo : int list list) (len : int) (last : char) : string = 
    if len > 0 then
      let hist = coo.[char2ind last] // Warning, this can give a index out-of-bound exception
      let next = randomChar hist
      string last + (appendRandomString coo (len - 1) next)
    else
      ""
  appendRandomString cooc len 'a'

let compareHist (left : int list) (right : int list) : double =
  let ssd = List.zip left right |> List.fold (fun acc (l,r) -> acc + (pown (l - r) 2)) 0
  (float ssd)/(float left.Length)
  
let compareCooccurrence (left : int list list) (right : int list list) : double =
  let ssd = List.zip left right |> List.fold (fun acc (l,r) -> acc + compareHist l r) 0.0
  ssd/(float left.Length)
  
// Types to make the sematics clearer
type wordHistogram = (string * int) list
type wordCooccurrences = (string * wordHistogram) list

/// A histogram of words. Return value is sorted a list of pairs of a word and its count in the input text
let wordHistogram (str : string) : wordHistogram =
  str.Split ' ' |> Array.toList |> List.countBy id |> List.sortBy fst

/// Take the square difference between word histograms
let rec diffw (a : wordHistogram) (b : wordHistogram) : int =
  match (a, b) with
    | ([], (v, w)::bRst) -> w*w + diffw a bRst
    | ((p, q)::aRst, []) -> q*q + diffw aRst b
    | ((p, q)::aRst, (v,w)::bRst) ->
       if p < v then
         q*q + diffw aRst b
       elif p > v then
         w*w + diffw a bRst
       else
         let d = q-w
         d*d + diffw aRst bRst
    | _ -> 0

/// Generate a random word according to the histogram of words in wHist
let randomWord (wHist : wordHistogram) : string =
  let (alph,count) = List.unzip wHist // This is inefficient, but the representation has advantages for cooccurrences
  let cumHist = cumSum count
  let v = rnd.Next(cumHist.[cumHist.Length-1])
  let i = reverseLookup cumHist v
  alph.[i] // Warning, this may cause an index out-of-bound exception

/// Generate a sentence of random words separated by spaces
let randomWords (wHist : wordHistogram) (len : int) : string =
  String.init len (fun _ -> randomWord wHist + " ")

/// Count the cooccurrences of words in a string and return a list of pairs of a word and the observed words that follow including their counts.
let cooccurrenceOfWords (str : string) : wordCooccurrences = 
  let words = str.Split ' ' |> Array.toList
  let alph = words |> List.distinct |> List.sort

  let rec neighbourOf (lst : string list) (w : string) : string list =
    match lst with
      a::b::rst when a = w -> b::(neighbourOf (b::rst) w)
      | a::rst -> neighbourOf rst w
      | _ -> []
      
  let neighbourOfHist (words : string list) (w : string) : (string * int) list =
    neighbourOf words w |> List.countBy id

  List.map (fun w -> (w, neighbourOfHist words w)) alph

let rec diffw2 (a : wordCooccurrences) (b : wordCooccurrences) : int =
  match (a, b) with
    | ([], (v, w)::bRst) -> diffw [] w + diffw2 a bRst
    | ((p, q)::aRst, []) -> diffw q [] + diffw2 aRst b
    | ((p, q)::aRst, (v,w)::bRst) ->
       if p < v then
         diffw q [] + diffw2 aRst b
       elif p > v then
         diffw [] w + diffw2 a bRst
       else
         diffw q w + diffw2 aRst bRst
    | _ -> 0
    

/// Generate a random sentence with the same cooccurrence statistics as cooc.
let fstOrderMarkovModelOfWords (cooc: wordCooccurrences) (nWords: int) : string =
  let rec appendRandomString (c : wordCooccurrences) (nWords : int) (last : string) : string = 
    if nWords > 0 then
      let (w, hist) = List.find (fun (elm, hist) -> elm = last) c
      let next = randomWord hist
      string last + " " + (appendRandomString c (nWords - 1) next)
    else
      ""
  appendRandomString cooc nWords "once"

let ngram (lst : 'a list) (n : int) : Map<'a list, ('a * int) list> =
  let rec populate (dict : Map<'a list, 'a list>) (lst : 'a list) (n : int) : Map<'a list, 'a list> =
    if lst.Length > n then
      let key = lst.[0..(n-1)]
      let obs = lst.[n]
      let obsLst =
        match dict.TryFind key with
          Some aLst -> aLst
          | None -> []
      populate (dict.Add (key, obs::obsLst)) lst.[1..] n // Add adds or replaces a key-value pair and returns a new dictionary
    else
      dict
  let aMap = populate (Map.empty<'a list, 'a list>) lst n
  Map.map (fun key lst -> lst |> List.countBy id |> List.sortBy fst) aMap // Sort each of the histograms

let randomElm (hist : ('a * int) list) : 'a =
  let cumHist = hist |> List.map snd |> cumSum
  let v = rnd.Next(cumHist.[cumHist.Length-1])
  let i = reverseLookup cumHist v
  hist.[i] |> fst

let randomText (dict: Map<'a list, ('a * int) list>) (def: ('a * int) list) (key : 'a list) (len: int) (verbose : bool): 'a list =
  let rec appendRandomString (dict: Map<'a list, ('a * int) list>) (def: ('a * int) list) (key : 'a list) (len: int) (verbose : bool) : 'a list =
    if len - key.Length > 0 then
      let hist =
        if key.IsEmpty then
          def
        else
          match dict.TryFind key with
            Some aLst -> aLst
            | None -> def
      let nextElm = randomElm hist
      if verbose then
        printfn "%A : %A -> %A" key hist nextElm
      if key.IsEmpty then
        nextElm :: (appendRandomString dict def key (len - 1) verbose)
      else
      // key is a running window of the last n elements, nextElm is the next and the first in key is removed.
        key.Head :: (appendRandomString dict def (key.Tail@[nextElm]) (len - 1) verbose)
    else
      key
  appendRandomString dict def key len verbose
