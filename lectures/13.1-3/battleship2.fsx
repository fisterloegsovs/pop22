/// A game is a battleship game
type game() =
  let p1 = player()
  let p2 = player()

/// A player is a human player, which has 2 boards and
/// several ships
and player() =
  let my_board = board()
  let opponents_board = board()

// ...
  
/// An opponent is another player
and opponent() = class end

/// A ship can be damaged
and ship() = class end

/// A board is a square set of fields with row-column
/// coordinates. Ships are placed on the board
and board() =
  let fields = Array2D.init 10 10 (fun i j -> field (coordinate (i, j)))

/// A field is can be covered by a ship and can have been
/// shootend at
and field(c : coordinate) =
  member this.coord with get () = c

/// A coordinate is a location on a board
and coordinate(row : int, col : int) =
  member this.row with get () = row
  member this.col with get () = col
