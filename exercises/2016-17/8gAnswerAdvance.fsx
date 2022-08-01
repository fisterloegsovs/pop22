type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

let rnd = System.Random ()
let codeColorList : codeColor list = [Red; Green; Yellow; Purple; White; Black]

let rec histogram bins list =
  match bins with
    | b::bs -> let l = (List.filter (fun elem -> elem = b) list) in l.Length :: (histogram bs list)
    | _ -> []

let validate (a : code) (g : code) : answer =
  let black = List.sum (List.map2 (fun elm1 elm2 -> if elm1 = elm2 then 1 else 0) a g)

  let aHist = histogram codeColorList a
  let gHist = histogram codeColorList g
  let white = List.sum (List.map2 (fun elm1 elm2 -> min elm1 elm2) aHist gHist)
    
  (white - black, black)

let getHumanOrComputer (question : string) =
  printf "%s" question
  let c = char (System.Console.Read ())
  printfn ""
  if c = 'H' || c = 'h' then
    Human
  else
    Computer

let getCode () : code = 
  let getColorCode () = 
    let c = char (System.Console.Read ()) 
    match c with
      | 'R' | 'r' -> Some Red
      | 'G' | 'g' -> Some Green
      | 'Y' | 'y' -> Some Yellow
      | 'P' | 'p' -> Some Purple
      | 'W' | 'w' -> Some White
      | 'B' | 'b' -> Some Black
      | _ -> None
      
  let rec getCodeHelper n : code =
    if n > 0 then
      printf "%d: " n
      let c = getColorCode ()
      printf " "
      if c.IsNone then 
          getCodeHelper n
      else
        (getCodeHelper (n-1)) @ [c.Value]
    else
      []
    
  let c = getCodeHelper 4
  printfn ""
  c

let guess (p : player) (b : board) : code =
  if p = Computer then
    let rec listProd (aCode : code) (aCodeList : code list) =
      match aCode with
        | elm::rest -> (List.map (fun subList -> elm :: subList) aCodeList) @ (listProd rest aCodeList)
        | _ -> []

    let rec autoListProdN (l : codeColor list) n =
      if n > 0 then
        listProd l (autoListProdN l (n-1))
      else
        [[]]

    let rec sieve (aCodeList : code list) (b : board) : code list=
      let filterValidPossibility (aGuess, anAnswer) aPossibleGuess =
        (aGuess <> aPossibleGuess) && (validate aGuess aPossibleGuess) = anAnswer
        
      match b with
        | line :: rest -> sieve (List.filter (filterValidPossibility line) aCodeList) rest
        | _ -> aCodeList

    let all = autoListProdN codeColorList 4
    let remaining = sieve all b

    remaining.[rnd.Next(remaining.Length)]
  else
    printf "Type 4 colors (RGYPWB). "
    getCode ()
    
let makeCode (p : player) : code =
  guess p (List.empty<code * answer>)

let mutable b = List.empty<code * answer>

let player1 = getHumanOrComputer "Who plays the task giver (H/C)? "
let player2 = getHumanOrComputer "Who plays the task solver (H/C)? "

printfn "Taks giver:"
let a = makeCode player1

printfn "Task solver:"
let mutable v = (0,0)
while v <> (0,4) do
  let g = guess player2 b
  v <- validate a g
  b <- (g, v) :: b
  printfn "%A : %d -> %A -> %A" a b.Length g v
