module animals

type symbol = char
type position = int * int
type neighbour = position * symbol

let mSymbol : symbol = 'm'
let wSymbol : symbol = 'w'
let eSymbol : symbol = ' '
let rnd = System.Random ()

/// An animal is a base class. It has a position and a reproduction counter.
type animal (symb : symbol, repLen : int) =
  let mutable _reproduction = rnd.Next(1,repLen)
  let mutable _pos : position option = None
  let _symbol : symbol = symb

  member this.symbol = _symbol
  member this.position
    with get () = _pos
    and set aPos = _pos <- aPos
  member this.reproduction = _reproduction
  member this.updateReproduction () =
    _reproduction <- _reproduction - 1
  member this.resetReproduction () =
    _reproduction <- repLen
  override this.ToString () =
    string this.symbol

/// A moose is an animal
type moose (repLen : int) =
  inherit animal (mSymbol, repLen)

  member this.updateReproduction () =
    base.updateReproduction ()
    if this.reproduction > 0 then
      None
    else // Give birth
      this.resetReproduction ()
      Some (moose (repLen))
  member this.tick () =
    this.updateReproduction ()

/// A wolf is an animal with a hunger counter
type wolf (repLen : int, hungLen : int) =
  inherit animal (wSymbol, repLen)
  let mutable _hunger = hungLen

  member this.updateReproduction () =
    base.updateReproduction ()
    if this.reproduction > 0 then
      None
    else // Give birth
      this.resetReproduction ()
      Some (wolf (repLen, hungLen))
  member this.hunger = _hunger
  member this.resetHunger () =
    _hunger <- hungLen
  member this.updateHunger () =
    _hunger <- _hunger - 1
    if _hunger <= 0 then
      this.position <- None // Starve to death
  member this.tick () =
    this.updateHunger () // Hunger is updated even for the dead
    match this.position with
      None -> None
      | _ -> this.updateReproduction ()

type board =
  {width : int;
   mutable moose : moose list;
   mutable wolves : wolf list;}

/// An environment is a chess-like board with all animals and implenting all rules
type environment (boardWidth : int, NMooses : int, mooseRepLen : int, NWolves : int, wolvesRepLen : int, wolvesHungLen : int, verbose : bool) =
  let _board : board = {
    width = boardWidth;
    moose = List.init NMooses (fun i -> moose(mooseRepLen));
    wolves = List.init NWolves (fun i -> wolf(wolvesRepLen, wolvesHungLen));
  }
  
  let draw (b : board) : char [,] =
    let arr = Array2D.create<char> boardWidth boardWidth eSymbol
    for m in b.moose do
      Option.iter (fun p -> arr.[fst p, snd p] <- mSymbol) m.position
    for w in b.wolves do
      Option.iter (fun p -> arr.[fst p, snd p] <- wSymbol) w.position
    arr

  let anyEmptyField (b : board) : position =
    let arr = draw b
    let mutable i = rnd.Next b.width
    let mutable j = rnd.Next b.width
    while arr.[i,j] <> eSymbol do
      i <- rnd.Next b.width
      j <- rnd.Next b.width
    (i,j)

  let neighbours (b : board) (pos : position) : neighbour list =
    let arr = draw b
    let rec augment delta =
      match delta with
        d :: rest ->
          let di = fst pos + fst d
          let dj = snd pos + snd d
          if di < 0 || di > b.width-1 || dj < 0 || dj > b.width-1 then
            augment rest
          else
            ((di,dj), arr.[di,dj]) :: augment rest
        | _ -> []
    augment [(-1,1); (0,1); (1,1); (-1,0); (1,0); (-1,-1); (0,-1); (1,-1)]

  let anyEmptyNeighbour (lst : neighbour list) : position option =
    let shortened = List.filter (fun e -> snd e = eSymbol) lst
    if List.isEmpty shortened then
      None
    else
      let i = rnd.Next shortened.Length
      Some (fst shortened.[i])

  let updateMoose (b : board) (m : moose) : (moose option * string option) =
    // Side-effect warning! m may change
    // Mooses can move and reproduce
    let offspring = m.tick ()
    if m.position.IsSome then
      let neigh = neighbours b m.position.Value
      let newPos = anyEmptyNeighbour neigh
      if offspring.IsSome then // reproduce
        if newPos.IsSome then
          (Option.map (fun (e : moose) -> e.position <- newPos; e) offspring, Some "New calf")
        else
          (None, Some "Calf died")
      else // move
        m.position <- newPos
        (None,None)
    else
      (None,None)
        
  let anyMooseNeighbour (lst : neighbour list) : position option =
    let shortened = List.filter (fun e -> snd e = mSymbol) lst
    if List.isEmpty shortened then
      None
    else
      let i = rnd.Next shortened.Length
      Some (fst shortened.[i])

  let findMoose (b : board) (p : position option) : moose =
    let samePosition p q =
      match p, q with
        (Some a, Some b) -> a = b
        | _ -> false
    List.find (fun e -> samePosition p e.position) b.moose // potential exception
        
  let updateWolf (b : board) (w : wolf) : (wolf option * string option) =
    // Side-effect warning! b and w may change
    // Wolves can move, reproduce, and eat
    let offspring = w.tick ()
    if w.position.IsSome then
      let neigh = neighbours b w.position.Value
      let newPos = anyEmptyNeighbour neigh
      if offspring.IsSome then // reproduce
        if newPos.IsSome then
          (Option.map (fun (e : wolf) -> e.position <- newPos; e) offspring, Some "New cub")
        else
          (None, Some "Cub died")
      else // move and possibly eat
        let nearbyMoose = anyMooseNeighbour neigh
        if nearbyMoose.IsSome then // eat
          let m = findMoose b nearbyMoose
          if verbose then
            printfn "Moose eaten"
          w.position <- m.position
          m.position <- None
          w.resetHunger ()
        else
          w.position <- newPos
        (None,None)
    else
      (None,None)

  let rec processLists (mLst : moose list, wLst : wolf list) : unit =
    let doMoose m =
      let (calf, msg) = updateMoose _board m;
      Option.iter (fun e -> _board.moose <- e :: _board.moose) calf
      if verbose then
        Option.iter (fun e -> printfn "%s" e) msg
    let doWolf w =
      let (cub, msg) = updateWolf _board w;
      Option.iter (fun e -> _board.wolves <- e :: _board.wolves) cub
      if verbose then
        Option.iter (fun e -> printfn "%s" e) msg
    
    match (mLst, wLst) with
      ([], []) -> ()
      | (m::mRest, []) -> doMoose m; processLists (mRest, [])
      | ([], w::wRest) -> doWolf w; processLists ([], wRest)
      | (m::mRest, w::wRest) ->
        if rnd.Next 2 > 0 then
          doMoose m; processLists (mRest, wLst)
        else
          doWolf w; processLists (mLst, wRest)

    _board.moose <- List.filter (fun e -> e.position.IsSome) _board.moose
    _board.wolves <- List.filter (fun e -> e.position.IsSome) _board.wolves

  do for m in _board.moose do
       m.position <- Some (anyEmptyField _board)
  do for w in _board.wolves do
       w.position <- Some (anyEmptyField _board)

  member this.size = boardWidth*boardWidth
  member this.count = _board.moose.Length + _board.wolves.Length
  member this.board = _board
  member this.tick () = processLists (_board.moose, _board.wolves)
  override this.ToString () =
    let arr = draw _board
    let mutable ret = "  "
    for j = 0 to _board.width-1 do
      ret <- ret + string (j % 10) + " "
    ret <- ret + "\n"
    for i = 0 to _board.width-1 do
      ret <- ret + string (i % 10) + " "
      for j = 0 to _board.width-1 do
        ret <- ret + string arr.[i,j] + " "
      ret <- ret + "\n"
    ret

