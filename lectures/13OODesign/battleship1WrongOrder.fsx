
/// A game is a battleship game
type game() =
  let p1 = player()
  let p2 = player()

/// A player is a human player, which has 2 boards and
/// several ships
type player() =
  let my_board = board()
  let opponents_board = board()

// ...

/// An opponent is another player
type opponent() = class end

/// A ship can be damaged
type ship() = class end

/// A board is a square set of fields with row-column
/// coordinates. Ships are placed on the board
type board() =
  let j = 1
  let fields = List.fold (fun acc i -> field (coordinate (i, j))) [] [1..10]

/// A field is can be covered by a ship and can have been
/// shootend at
type field(c : coordinate) =
  member this.coord with get () = c

/// A coordinate is a location on a board
type coordinate(row : int, col : int) =
  member this.row with get () = row
  member this.col with get () = col
