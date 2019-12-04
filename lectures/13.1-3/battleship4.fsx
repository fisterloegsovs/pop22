/// A board is a square set of fields with row-column
/// coordinates. Ships are placed on the board
type board() =
  let fields = Array2D.init 10 10 (fun i j -> None)

/// A ship can be damaged
type ship() = class end

/// A player is a human player, which has 2 boards and
/// several ships
type player() =
  let my_board = board()
  let opponents_board = board()
  let mutable opponent = None
  member this.setOpponent(p : player) =
    opponent <- Some p

/// A game is a battleship game
type game() =
  let p1 = player()
  let p2 = player()
  do p1.setOpponent(p2)
  do p2.setOpponent(p1)
