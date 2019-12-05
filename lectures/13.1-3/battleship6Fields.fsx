/// A coordinate type is a row-column pair
type coordinate = int*int

/// A ship can be damaged
type ship(sz : int) =
  let mutable noHits = 0
  member this.size = sz
  member this.hit = noHits <- noHits + 1
  member this.isSunk = noHits >= sz
  
/// A field's state may have been shot at and may be occupied by a
/// ship
type State = bool * (ship option)

/// A board is a square set of fields with row-column
/// coordinates. Ships are placed on the board
type board() =
  let fields = Array2D.create 10 10 ((false, None) : State)
  member this.rows = Array2D.length1 fields
  member this.cols = Array2D.length2 fields
  member this.setShip (s : ship) (coords : (int*int) list) : bool =
    let isRoom = List.forall (fun (i,j) -> snd fields.[i,j] = None ) coords
    if isRoom then
      List.iter (fun (i,j) -> fields.[i,j] <- (fst fields.[i,j], Some s)) coords
    isRoom

/// A player is a human player, which has 2 boards and
/// several ships
type player() =
  let my_board = board()
  let opponents_board = board()
  let mutable opponent = None
  let ships = List.map (fun elm -> ship(elm)) [2;2;3;5]
  let rnd = System.Random()
  let setShip (s : ship) =
    let mutable success = false
    while not success do
      let i = rnd.Next(my_board.rows)
      let j = rnd.Next(my_board.cols - s.size)
      let coords = List.map (fun elm -> (i,j+elm)) [0..s.size-1]
      success <- my_board.setShip s coords
  do List.iter (fun s -> setShip s) ships 
  member this.setOpponent(p : player) =
    opponent <- Some p

/// A game is a battleship game
type game() =
  let p1 = player()
  let p2 = player()
  do p1.setOpponent(p2)
  do p2.setOpponent(p1)
