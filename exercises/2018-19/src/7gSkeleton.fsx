// Initial board, first 7 is player 1's 1-6 and home,
// second 7 is player 2's
let board = [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|]
type player = Player1 | Player2

/// Print the board, player 1 is bottom row and rightmost
/// home.
let printBoard (board : int []) : unit =
  ...

/// Convert from user's coordinates to board's coordinates.
/// Players enters 1-6 corresponding to board index 0-5
/// or 7-12
let pit2ind (p : player) (pit : int) : int =
  ...

/// True if either side has no beans
let isGameOver (board : int []) : bool =
  ...

/// True if board index is the players home
let isHome (p : player) (ind : int) : bool =
  ...

/// Get the index of next move. User must enter a pit
/// number between 1-6 corresponding to a
/// non-empty board index
let rec getMove (p : player) (str : string) : int option =
  ...

/// Updates board by distributing beans counter clockwise,
/// capturing when relevant. Board coordinate of last bean
/// is returned. Sideeffect warning!
let distributeNUpdate (board : int []) (ind : int) : int =
  ...
     
