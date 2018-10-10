let board = [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|]

/// Print the board, user one is bottom row and rightmost home
let printBoard (board : int []) =
  printfn "%6d%3d%3d%3d%3d%3d" board.[12] board.[11] board.[10] board.[9] board.[8] board.[7]
  printfn "%3d%21d" board.[13] board.[6]
  printfn "%6d%3d%3d%3d%3d%3d" board.[0] board.[1] board.[2] board.[3] board.[4] board.[5]

/// Convert from user's coordinates to board's coordinates
/// Players enters 1-6 corresponding to board index 0-5 or 7-12
let user2board (player : int) (compartment : int) : int =
  if player = 1 then compartment - 1 else (compartment - 1 + 7)

/// Convert from board's coordinates to user's coordinates
/// Board index 0-5 and 7-12 corresponds to user's 1-6
let board2user (cmp : int) : int =
  if cmp < 7 then cmp + 1 else (cmp + 1 - 7)

/// True if board coordinate is the players home
let isHome (player : int) (cmp : int) : bool =
  if player = 1 && cmp = 6 || player = 2 && cmp = 13 then true else false

/// Get the user's coordinate of next move. Selected compartment must be a number between 1-6 and non-empty
let getMove (cnvt : int -> int) (str : string) : int =
  let mutable noLegalMove = true
  let mutable compartment = -1
  while noLegalMove do
    printf "%s" str
    compartment <- int (System.Console.ReadLine ()) |> cnvt
    noLegalMove <- if (0 <= compartment && compartment <= 5 || 7 <= compartment && compartment <= 12) && board.[compartment] > 0 then false else true
  compartment

/// Distribute beans counter clockwise 
let distributeBeans (board : int []) (home : int) (cmp : int) : int =
  printfn "distributeBeans: %d %d" cmp board.[cmp]
  let capture cmp =
    printfn "capture: %d %d" cmp board.[cmp]
    if cmp <> 6 && cmp <> 13 && board.[cmp] = 1 then
      let oppositePlayer = if cmp < 7 then 2 else 1
      let opposite = 6 - (board2user cmp) + 1 |> user2board oppositePlayer
      printfn "capture2: %d %d %d" oppositePlayer (board2user cmp) opposite
      let n = board.[opposite];
      board.[opposite] <- 0
      board.[home] <- board.[home] + n
    else
      ()

  let n = board.[cmp]
  board.[cmp] <- 0  
  let mutable nextCmp = - 1
  for i = 1 to n do
    nextCmp <- (cmp + i) % 14
    board.[nextCmp] <- board.[nextCmp] + 1
  capture nextCmp
  nextCmp

while true do
  for i = 1 to 2 do
    printBoard board
    let cnvt = user2board i
    let str = sprintf "Player %d's move? " i
    let mutable compartment = getMove cnvt str
    let home = if i = 1 then 7 else 13
    while (distributeBeans board home compartment |> isHome i) do
      printBoard board
      let str = sprintf "Again? "
      compartment <- getMove cnvt str    
    
