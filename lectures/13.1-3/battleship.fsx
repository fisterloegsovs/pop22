/// A coordinate type is a row-column pair
type coordinate = int*int

/// A ship has a size and keeps track of the number of  hits, it has
/// received.
type ship(sz : int) =
  let mutable _noHits = 0

  /// the size of a ship
  member this.size = sz
  /// Increase the number of hits a ship has taken
  member this.hit () = _noHits <- _noHits + 1
  /// Check if a ship has taken more hits than its size
  member this.isSunk () = _noHits >= sz
  
/// A field's state may have been shot at and may be occupied by a
/// ship
type State = bool * (ship option)

/// A board is a square set of fields with row-column coordinates.
type board (sz : coordinate) =
  // A board is a 2d array of fields, each with a state of having been
  // shot at or not, and an optional reference to a ship object.
  let (_rows, _cols) = sz
  let _fields = Array2D.create _rows _cols ((false, None) : State)

  /// The number of _rows on the board
  member this.rows = _rows
  /// The number of columns on the board
  member this.cols = _cols
  /// Check if a list of coordinats is without a ship
  member this.isEmpty (coords : coordinate list) : bool =
    List.forall (fun (i,j) -> snd _fields.[i,j] = None ) coords
  /// Fill coordinates with a single ship
  member this.setShip (s : ship, coords : coordinate list) : unit =
    List.iter (fun (i,j) -> _fields.[i,j] <- (fst _fields.[i,j], Some s)) coords
  /// Register a shot taken at a field
  member this.registerShot (coords : coordinate) =
    let s = snd (_fields.[fst coords, snd coords])
    _fields.[fst coords, snd coords] <- (true, s)
  /// Register a shot taken at a field and change the ship reference
  member this.registerShot (coords : coordinate, s : ship option) =
    _fields.[fst coords, snd coords] <- (true, s)
  /// Accept a shot. If the coordinate contains a ship reference, and
  /// it has not been shot at before, then increase the shot-count of
  /// that ship
  member this.shootAt (coords : coordinate) : bool =
    let (b,s) = _fields.[fst coords, snd coords]
    if not b then Option.iter (fun (elm : ship) -> elm.hit ()) s
    this.registerShot coords
    s.IsSome
  /// Produce a string-representation of the board as a table with the
  /// sympols " " - no shot taken and no ship, "s" - no shot take and
  /// a ship present, "m" - shot take but not ship present, "h" shot
  /// take and ship hit.
  override this.ToString () =
    let digits = max _rows _cols |> float |> log10 |> ceil |> int
    let mutable str = (String.replicate (digits+1) " ")
    for j = 1 to _cols do
      str <- str + (sprintf "%*d " digits j)
    str <- str + "\n"
    for i = 1 to _rows do
      str <- str + (sprintf "%*d " digits i)
      for j = 1 to _cols do
        let sym =
          match _fields.[i-1,j-1] with
            (true, None ) -> "m"
            | (true, Some s) -> "h"
            | (false, Some s) -> "s"
            | _ -> " "
        str <- str + (sprintf "%*s " digits sym)
      str <- str + "\n"
    str.[..str.Length-2]
      
/// A player is a human player, which has 2 boards and
/// several ships
type player (name : string, sz : coordinate, ships : int list) =
  // The player's name
  let _name = name
  // The player's board
  let _myBoard = board sz
  // The player's knowledge about the opponent's board
  let _opponentsBoard = board sz
  // The player's list of ships
  let _ships = List.map (fun elm -> ship(elm)) ships
  // A dummy ship for accounting for hits at the opponent's board
  let _dummy = ship 1
  // The random number generator for initial placement of ships 
  let _rnd = System.Random()
  // Find a random vacant place for a given ship.
  let setShip (s : ship) =
    let mutable success = false
    while not success do
      let i = _rnd.Next(_myBoard.rows)
      let j = _rnd.Next(_myBoard.cols - s.size)
      let coords = List.map (fun elm -> (i,j+elm)) [0..s.size-1]
      success <- _myBoard.isEmpty coords
      if success then
        _myBoard.setShip (s, coords)
  // All ships are placed randomly on the player's board at instantiation.
  do List.iter (fun elm -> setShip elm) _ships 

  /// Get the player's board
  member this.myBoard with get () = _myBoard
  /// Get the player's knowledge about the opponent's board
  member this.opponentsBoard with get () = _opponentsBoard
  /// Register a shot and it's result on the copy of the opponent's board
  member this.registerShot (c : coordinate, res : bool) =
    let s = if res then Some _dummy else None
    _opponentsBoard.registerShot (c, s)
  /// Accept a shot from the opponent
  member this.shootAt c =
    _myBoard.shootAt c
  /// Check if the player has any ships left
  member this.anyShipsLeft () =
    not (List.forall (fun (elm : ship) -> elm.isSunk ()) _ships)
  /// Ask the user for a board coordinate. Coordinates start from (1,1).
  member this.getCoordinates () =
    printf "Enter row col: "
    let str = System.Console.ReadLine ()
    let arr = str.Split ' ' |> Array.filter (fun (elm : string) -> elm.Length > 0)
    (int arr.[0] - 1, int arr.[1] - 1) 
  /// Produce a string representation of the player's name
  override this.ToString () = _name

/// A game is a battleship game. It has a board size and a list of
/// ship and their lengths
type game (sz : coordinate, ships : int list) =
  // Initialize two players
  let _p0 = player("Player 1", sz, ships)
  let _p1 = player("Player 2", sz, ships)

  /// The main loop, which continues until one player has no
  /// more ships
  member this.play () =
    /// Perform a turn with p being the player and o the opponent of
    /// that turn
    let rec aTurn (p : player) (o : player) : unit =
      if p.anyShipsLeft () && o.anyShipsLeft () then
        printfn "\n%A:\nYou\n%A\nOpponent\n%A" p p.myBoard p.opponentsBoard
        let c = p.getCoordinates ()
        let res = o.shootAt c
        printfn "Hit: %b" res
        p.registerShot (c, res)
        aTurn o p
    aTurn _p0 _p1

// Instantiate a game object and play the game 
let g = game((10,10), [2;2;3;5])
g.play()
