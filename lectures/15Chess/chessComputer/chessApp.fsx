open Chess
open Pieces

type Move = Position * Position
type ScoredMove = Move * float
type Runs = Position list * chessPiece list

/// Create a game with pieces
let createGame () : board =
  // Create a game
  let b = board () // Create a board
  // Pieces are kept in an array for easy testing
  let pieces = [|
    king (White) :> chessPiece;
    rook (White) :> chessPiece;
    king (Black) :> chessPiece;
    rook (Black) :> chessPiece |]
  // Place pieces on the board
  b.[0,0] <- Some pieces.[0]
  b.[1,1] <- Some pieces.[1]
  b.[4,1] <- Some pieces.[2]
  b.[3,1] <- Some pieces.[3]
  b

let rnd = System.Random()

/// Make an optimal move for a given color. If the board is unchanged,
/// then now valid move is possible.
let takeTurn (b : board) (c : Color) : board =
  // Make a copy of the board where a move have been performed
  let move (b : board) (src : Position) (pos : Position) : board =
    let newB = b.copy()
    newB.move src pos
    newB

  // Convert the list of neighbouring opposing pieces to position and
  // merge with vacant positions
  let availabeMoves2Pos (c : Color) ((vacant, neighbours) : Runs) : Position list =
      let getOpposingPos (c : Color) (acc : Position list) (elm : chessPiece) =
          if elm.color = (opposingColor c) then
              elm.position.Value :: acc
          else
              acc
      let posOpposingNeighbours = List.fold (getOpposingPos c) [] neighbours
      vacant @ posOpposingNeighbours

  // Apply a function to each element of a list and pick one from the
  // result.
  let tryMapNPick (mapFct : 'a -> 'b) (pickFct : 'b list -> 'b) (lst : 'a list) : 'b option =
    try
      lst |> List.map mapFct |> pickFct |> Some
    with
      _ -> None 

  // Find the maximum value of scores
  let tryMaxScore (lst : ScoredMove list) : float option =
    tryMapNPick snd List.max lst
    
  // Find the minimum value of scores
  let tryMinScore (lst : ScoredMove list) : float option =
    tryMapNPick snd List.min lst

  let filterByScores (lst : ('a * 'b) list) (v : 'b option) : ('a * 'b) list =
    match v with
      None -> []
      | Some s -> List.filter (fun (pair, score) -> s = score) lst

  // Evaluate all moves for a list of chessPieces and evaluate their
  // resulting score. The result is a single, flat list of
  // ((src,dest),score) elements including all moveable pieces.
  let rec scoreMoves (b : board) (pieces : chessPiece list) : ScoredMove list = 
    printfn "pieces: %A" pieces
    match pieces with
      [] -> List<ScoredMove>.Empty
      | p :: rst -> 
        let srcOption = p.position
        let moves =
          match srcOption with
            None -> []
            | Some src ->
              // Add the opponent's neighbouring pieces's positions
              let choises = p |> b.availableMoves |> availabeMoves2Pos p.color
              let posPairs = List.map (fun dst -> (src,dst)) choises
              let evalScore pair =
                let tmpBoard = move b (fst pair) (snd pair)
                tmpBoard.score ()
              let scores = List.map evalScore posPairs
              List.zip posPairs scores
        moves @ (scoreMoves b rst)

  // Pick an optimal 1-step ahead move
  let pickMove (c : Color) (moves : ScoredMove list) : Move option =
    printfn "moves: %A" moves
    // White wants to maximize score, black minimize
    let optVal =
      if c = White then
        tryMaxScore moves
      else
        tryMinScore moves
    printfn "optVal: %A" optVal
    // list of all equally good moves
    let goodMoves = filterByScores moves optVal
    printfn "goodMoves: %A" goodMoves
    if goodMoves.Length > 0 then
      let idx = rnd.Next goodMoves.Length;
      let ((src, dst), score) = goodMoves.[idx]
      Some (src,dst)
    else
      None

  // find a good move if possible
  let m =
    b.pieces ()
    |> List.filter (fun p -> p.color = c)
    |> scoreMoves b
    |> pickMove c
  match m with
    None -> b
    | Some (src,dst) -> printfn "move: %A->%A" src dst; move b src dst

/// Play n turns starting with a given board and a given color
let rec play (b : board) (n : int) (c : Color) : board =
  printfn "Board:\n%A\nScore: %A" b (b.score ())
  match n with
    0 ->
        b
    | _ ->
        printfn "Turns left:%d\nPlayer: %A" n c
        let newB = takeTurn b c
        let nextC = opposingColor c
        play newB (n-1) nextC

let initialBoard = createGame ()
let finalBoard = play initialBoard 4 White
