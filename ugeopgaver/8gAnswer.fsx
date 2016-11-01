type codeColor = Red | Green | Blue | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

let rnd = System.Random ()
let codeColorList : codeColor list = [Red; Green; Blue; Purple; White; Black]

let guess (b : board) (p : player) : code =
  let rec guessHelper (b : board) n : code =
    if n > 0 then
      codeColorList.[rnd.Next(codeColorList.Length-1)] :: (guessHelper b (n-1))
    else
      []
  guessHelper b 4

let makeCode (p : player) : code =
  guess (List.empty<code * answer>) p

let rec histogram bins list =
  match bins with
    | b::bs -> let l = (List.filter (fun elem -> elem = b) list) in l.Length :: (histogram bs list)
    | _ -> []

let validate (a : code) (g : code) : answer =
  (*
  let mutable black = 0
  for i = 0 to a.Length - 1 do
    if a.[i] = g.[i] then
      black <- black + 1
  *)
  let black = List.sum (List.map2 (fun elm1 elm2 -> if elm1 = elm2 then 1 else 0) a g)

  let aHist = histogram codeColorList a
  let gHist = histogram codeColorList g
  (*
  let mutable white = 0
  for i = 0 to codeColorList.Length - 1 do
    white <- white + min aHist.[i] gHist.[i]
  *)
  let white = List.sum (List.map2 (fun elm1 elm2 -> min elm1 elm2) aHist gHist)
    
  (white - black, black)
      
let a = makeCode Computer
let mutable b = List.empty<code * answer>

for i = 0 to 10 do
  let g = guess b Computer
  let v = validate a g
  b <- (g, v) :: b
  printfn "%A : (%A, %A)" a g v
