// Initial board, first 7 is player 1's 1-6 and home, second 7 is
// player 2's
let board = [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|]
type player = Player1 | Player2

/// Print the board, player 1 is bottom row and rightmost home
let printBoard (board : int []) : unit =
  printfn "%6d%3d%3d%3d%3d%3d" board.[12] board.[11] board.[10] board.[9] board.[8] board.[7]
  printfn "%3d%21d" board.[13] board.[6]
  printfn "%6d%3d%3d%3d%3d%3d" board.[0] board.[1] board.[2] board.[3] board.[4] board.[5]

/// Convert from user's coordinates to board's coordinates Players
/// enters 1-6 corresponding to board index 0-5 or 7-12
let pit2ind (p : player) (pit : int) : int =
  if p = Player1 then pit - 1 else (pit - 1 + 7)

/// Convert from board's indices to pits (user's coordinates) Board
/// index 0-5 and 7-12 corresponds to pit 1-6 for player 1 and 2
/// respectively
let ind2pit (ind : int) : int =
  if ind < 7 then ind + 1 else (ind + 1 - 7)

/// True if either side has no beans
let isGameOver (board : int []) : bool =
  Array.forall (fun e -> e = 0) board.[0..5] || Array.forall (fun e -> e = 0) board.[7..12]

/// True if board index is the players home
let isHome (p : player) (ind : int) : bool =
  if p = Player1 && ind = 6 || p = Player2 && ind = 13 then true else false

/// Get the index of next move. User must enter a pit number between
/// 1-6 corresponding to a non-empty board index
let rec getMove (p : player) (str : string) : int option =
  printf "%s" str
  let ind = int (System.Console.ReadLine ()) |> pit2ind p
  if (0 <= ind && ind <= 5 || 7 <= ind && ind <= 12) && board.[ind] > 0 then
    Some ind
  else
    getMove p str

/// Updates board by distributing beans counter clockwise, capturing
/// when relevant. Board coordinate of last bean is
/// returned. Sideeffect warning!
let distributeNUpdate (board : int []) (ind : int) : int =
  /// If last bean lands in an empty pit, then the last bean and all
  /// beans in the opposite pit are captured.
  let captureNUpdateBoard (board : int []) (home : int) (ind : int) : unit =
    if ind <> 6 && ind <> 13 && board.[ind] = 1 then
      let oppositeP = if ind < 7 then Player2 else Player1
      let opposite = 6 - (ind2pit ind) + 1 |> pit2ind oppositeP
      let n = board.[opposite];
      if n > 0 then
        board.[opposite] <- 0
        board.[ind] <- 0
        board.[home] <- board.[home] + n + 1

  let home = if ind < 6 then 6 else 13
  let n = board.[ind]
  board.[ind] <- 0  
  let mutable nextInd = - 1
  for i = 1 to n do
    nextInd <- (ind + i) % 14
    board.[nextInd] <- board.[nextInd] + 1
  captureNUpdateBoard board home nextInd
  nextInd

// ////////////////////////////////////////////////////// //
let mutable p = Player2
while not (isGameOver board) do
  p <- if p = Player1 then Player2 else Player1
  printBoard board
  let str = sprintf "Player %A's move? " p
  let mutable ind = getMove p str
  while (Option.isSome ind) && (distributeNUpdate board ind.Value |> isHome p) && not (isGameOver board) do
    printBoard board
    let str = sprintf "Again? "
    ind <- getMove p str

let winnerStr =
  if board.[6] > board.[13] then
    "Player 1 wins."
  elif board.[6] = board.[13] then
    "It's a tie."
  else
    "Player 2 wins."

printfn "Game over: %s" winnerStr
