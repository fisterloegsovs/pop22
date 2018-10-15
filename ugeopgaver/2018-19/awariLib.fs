module Awari

type pit = int
type board = pit list
type player = Player1| Player2

// /////////////////////////////////////////////////// //
type index = int

/// Convert from user's pit-coordinates to board's
/// index-coordinates. Players enters pit number 1-6
/// corresponding to index number 0-5 or 7-12
let pit2ind (p : player) (i : pit) : index =
  if p = Player1 then i - 1 else (i - 1 + 7)

/// Convert from board's index 0-5 and 7-12 to user's pit number 1-6.
let ind2pit (i : index) : pit =
  if i < 7 then i + 1 else (i + 1 - 7)

/// Add 2 numbers and cyclicly map back to the interval 0..13.
let cyclicAdd (a : int) (b : int) = (a + b) % 14

/// Return the index of the opposing side or None if i is a home
let opposite (i : index) : index option =
  if i <> 6 && i <> 13 then
    let side = if i < 7 then Player2 else Player1
    let oppositePit = 6 - (ind2pit i) + 1
    let j = pit2ind side oppositePit
    Some j
  else
    None

/// Return true if i is larger than a and smaller than or equal b cyclicly
let inCyclicInterval a b i =
  //                     1 1 1 1
  // 0 1 2 3 4 5 6 7 8 9 0 1 2 3
  //       a - - - - b
  // - - b   a - - - - - - - - -
  (b < a && a < i) || (b < a && i <= b) || (b > a && a < i && i <= b)

/// (Re)build a board, updating bean counts accordingly
let rec build (partialB : board) (home : index) (a : index) (n : int) (oppPair : index * int ) (cur : index) : board =

  match partialB with
    | e :: rest ->
      let b = cyclicAdd a n
      let newE = 
        if cur = a then 0
        elif cur = b && snd oppPair <> 0 then 0
        elif cur = home && snd oppPair <> 0 && (inCyclicInterval a b cur) then e + 2 + (snd oppPair)
        elif cur = home && snd oppPair <> 0 && not (inCyclicInterval a b cur) then e + 1 + (snd oppPair)
        elif cur = fst oppPair && snd oppPair <> 0 then 0
        elif b < a && a < cur then e + 1
        elif b < a && cur <= b then e + 1
        elif a < cur && cur <= b then e + 1
        else e
      newE :: build rest home a n oppPair (cur+1)
        
    | _ -> []


// /////////////////////////////////////////////////// //

let printBoard (b : board) : unit =
  printfn "%6d%3d%3d%3d%3d%3d" b.[12] b.[11] b.[10] b.[9] b.[8] b.[7]
  printfn "%3d%21d" b.[13] b.[6]
  printfn "%6d%3d%3d%3d%3d%3d" b.[0] b.[1] b.[2] b.[3] b.[4] b.[5]

let isHome (b : board) (p : player) (i : pit) : bool =
  let ind = pit2ind p i
  p = Player1 && ind = 6 || p = Player2 && ind = 13

let isGameOver (b : board) : bool =
  List.forall (fun e -> e = 0) b.[0..5] || List.forall (fun e -> e = 0) b.[7..12]

let rec getMove (b : board) (p : player) (q : string) : pit =
  printf "%s" q
  let i = int (System.Console.ReadLine ())
  let ind = pit2ind p i
  if (1 <= i && i <= 6) && b.[ind] > 0 then
    i
  else
    getMove b p q

let rec distribute (b : board) (p : player) (i : pit) : board * player * pit =
  let ind = pit2ind p i
  let home = if ind < 6 then 6 else 13
  let finalInd = cyclicAdd ind b.[ind]
  let capture =
    let j = opposite (finalInd) // Index of opposing side of last bean
    if j.IsSome && (b.[finalInd] = 0) && (b.[j.Value] > 0) then (j.Value, b.[j.Value])
    else (0,0)
  let newB = build b home ind b.[ind] capture 0
  let finalPit = ind2pit finalInd
  let finalPitsPlayer = if finalInd < 7 then Player1 else Player2
  (newB, finalPitsPlayer, finalPit)
  
let turn (b : board) (p : player) : board =
  let rec helper (b : board) (p : player) (n : int) : board =
    printBoard b
    let str =
      if n = 0 then
        sprintf "Player %A's move? " p
      else 
        "Again? "
    let i = getMove b p str
    let (newB, finalPitsPlayer, finalPit) = distribute b p i
    if not (isHome b finalPitsPlayer finalPit) || (isGameOver b) then
     newB
    else
      helper newB p (n + 1)
  helper b p 0 

let rec play (b : board) (p : player) : board =
  if isGameOver b then
    b
  else
    let newB = turn b p
    let nextP =
      if p = Player1 then
        Player2
      else
        Player1
    play newB nextP
