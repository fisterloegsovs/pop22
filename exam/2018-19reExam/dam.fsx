type coord = int * int
type color = Black | White
type state = Man | King
type game () =
  /// Setup the game
  member this.intializeGame () = ()
  /// Let players take turn playing until no more movements is available for one or both of the players
  member this.play () = ()

type player (id : int, b : board) =
  /// Each player is identified by a unique id
  member this.id = id
  /// Put pieces on the board for initial lineup
  member this.putPiecesOnBoard board : unit = ()
  /// Get a move from a (human) player
  member this.getMove () : coord list = []
  /// Move a piece on the board
  member this.movePiece (cs : coord list) : unit = ()
  /// Check whether a player has any legal moves left
  member this.hasLegalMoves () : bool = true
and piece (c : color, b : board) =
  /// The piece's color
  member this.color = c
  /// The piece's state
  member this.state = Man
  /// Get the list of possible lists of moves for this piece
  member this.getMove b : coord list list = [[]]
and board (n : int) =
  /// An internal variable storing the optional pieces in an array of fields
  let fields = Array2D.create<piece option> n n (None)
  /// Put a piece on the board. None means clearing the field
  member this.setPiece (c : coord) (p : piece option) = ()
  /// Get the piece in on a field. None means an empty field
  member this.getPiece (c : coord) : piece option = None
  /// Show the present state of the board to the players.
  member this.draw () = ()
