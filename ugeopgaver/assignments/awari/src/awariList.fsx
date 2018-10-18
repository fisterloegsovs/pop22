// Initial board, first 7 is player 1's 1-6 and home, second 7 is
// player 2's
type board = int list
type player = Player1 | Player2
type index = int
type pit = int
let initialB : board = [3;3;3;3;3;3;0;3;3;3;3;3;3;0]
let UNITTEST = false;

/// Print the board, player 1 is bottom row and rightmost home
let printBoard (b : board) : unit =
  printfn "%6d%3d%3d%3d%3d%3d" b.[12] b.[11] b.[10] b.[9] b.[8] b.[7]
  printfn "%3d%21d" b.[13] b.[6]
  printfn "%6d%3d%3d%3d%3d%3d" b.[0] b.[1] b.[2] b.[3] b.[4] b.[5]

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
  
/// True if either side has no beans
let isGameOver (b : board) : bool =
  List.forall (fun e -> e = 0) b.[0..5] || List.forall (fun e -> e = 0) b.[7..12]

/// True if board index is the players home
let isHome (p : player) (i : index) : bool =
  if p = Player1 && i = 6 || p = Player2 && i = 13 then true else false

/// Get the index of next move. User must enter a pit number between
/// 1-6 corresponding to a non-empty board index
let rec getMove (b : board) (p : player) (str : string) : index =
  printf "%s" str
  let i = int (System.Console.ReadLine ()) |> pit2ind p
  if (0 <= i && i <= 5 || 7 <= i && i <= 12) && b.[i] > 0 then
    i
  else
    getMove b p str

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

/// Distributing beans counter clockwise, capturing
let rec distribute (b : board) (i : index) : board =
  let home = if i < 6 then 6 else 13
  let capture =
    let j = opposite (cyclicAdd i b.[i]) // Index of opposing side of last bean
    if j.IsSome && (b.[cyclicAdd i b.[i]] = 0) && (b.[j.Value] > 0) then (j.Value, b.[j.Value])
    else (0,0)
  build b home i b.[i] capture 0

/// Perform a possibly repeated turn of a player
let rec turn (b : board) (p : player) (n : int) : board =
  printBoard b
  let str =
    if n = 1 then
      sprintf "Player %A's move? " p
    else
      "Again? "
  let i = getMove b p str
  let newB = distribute b i
  let last = cyclicAdd i b.[i]
  if not (isHome p last) || (isGameOver b) then
    newB
  else
    turn newB p (n + 1)
    
/// Play game until one side is empty
let rec play (b : board) (p : player) : board =
  if isGameOver b then
    b
  else
    let newB = turn b p 1
    let nextP =
      if p = Player1 then
        Player2
      else
        Player1
    play newB nextP

// ////////////////////////////////////////////////////// //
if not UNITTEST then
  let finalB = play initialB Player1
  let winnerStr =
    if finalB.[6] > finalB.[13] then
      "Player 1 wins."
    elif finalB.[6] = finalB.[13] then
      "It's a tie."
    else
      "Player 2 wins."
      
  printfn "Game over: %s" winnerStr
else
  let test str b i truth =
    let newB = distribute b i
    printfn "%5b: %s" (newB = truth) str
    
  test "Initial setup choosing index 0"  [3;3;3;3;3;3;0;3;3;3;3;3;3;0]  0 [0;4;4;4;3;3;0;3;3;3;3;3;3;0]
  test "Initial setup choosing index 1"  [3;3;3;3;3;3;0;3;3;3;3;3;3;0]  1 [3;0;4;4;4;3;0;3;3;3;3;3;3;0]
  test "Initial setup choosing index 2"  [3;3;3;3;3;3;0;3;3;3;3;3;3;0]  2 [3;3;0;4;4;4;0;3;3;3;3;3;3;0]
  test "Initial setup choosing index 3"  [3;3;3;3;3;3;0;3;3;3;3;3;3;0]  3 [3;3;3;0;4;4;1;3;3;3;3;3;3;0]
  test "Initial setup choosing index 4"  [3;3;3;3;3;3;0;3;3;3;3;3;3;0]  4 [3;3;3;3;0;4;1;4;3;3;3;3;3;0]
  test "Initial setup choosing index 5"  [3;3;3;3;3;3;0;3;3;3;3;3;3;0]  5 [3;3;3;3;3;0;1;4;4;3;3;3;3;0]
  test "Initial setup choosing index 7"  [3;3;3;3;3;3;0;3;3;3;3;3;3;0]  7 [3;3;3;3;3;3;0;0;4;4;4;3;3;0]
  test "Initial setup choosing index 8"  [3;3;3;3;3;3;0;3;3;3;3;3;3;0]  8 [3;3;3;3;3;3;0;3;0;4;4;4;3;0]
  test "Initial setup choosing index 9"  [3;3;3;3;3;3;0;3;3;3;3;3;3;0]  9 [3;3;3;3;3;3;0;3;3;0;4;4;4;0]
  test "Initial setup choosing index 10" [3;3;3;3;3;3;0;3;3;3;3;3;3;0] 10 [3;3;3;3;3;3;0;3;3;3;0;4;4;1]
  test "Initial setup choosing index 11" [3;3;3;3;3;3;0;3;3;3;3;3;3;0] 11 [4;3;3;3;3;3;0;3;3;3;3;0;4;1]
  test "Initial setup choosing index 12" [3;3;3;3;3;3;0;3;3;3;3;3;3;0] 12 [4;4;3;3;3;3;0;3;3;3;3;3;0;1]
  test "Capture starting player 1 side landing player 1 side" [3;3;3;0;3;3;0;3;3;3;3;3;3;0]   0 [0;4;4;0;3;3;4;3;3;0;3;3;3;0]
  test "Capture starting player 1 side landing player 2 side" [3;3;3;3;3;3;0;3;0;3;3;3;3;0]   5 [3;3;3;3;0;0;5;4;0;3;3;3;3;0]
  test "Capture starting player 2 side landing player 2 side" [3;3;3;3;3;3;0;3;3;3;0;3;3;0]   7 [3;3;0;3;3;3;0;0;4;4;0;3;3;4]
  test "Capture starting player 2 side landing player 1 side" [3;0;3;3;3;3;0;3;3;3;3;3;3;0]  12 [4;0;3;3;3;3;0;3;3;3;3;0;0;5]
