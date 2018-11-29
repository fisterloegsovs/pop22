type environment (n : int, e : int, fe : int, u : int, fu : int, s : int) as this =
  let arr = Array2D.create<char> n n ' '
  let rnd = System.Random();
  let mutable mList = List.init e (fun i -> moose(fe, this))
  let mutable mDList : moose list = [];
  let mutable wList = List.init u (fun i -> wulf(s, fe, this))
  let mutable wDList : wulf list = [];
  do for m in mList do
    let (i,j) = this.anyEmptyField ()
    arr.[i,j] <- 'm'
    m.position <- Some (i,j)
  do for w in wList do
    let (i,j) = this.anyEmptyField ()
    arr.[i,j] <- 'u'
    w.position <- Some (i,j)
  member this.size = n
  member this.Item
    with get (i : int, j : int) = arr.[i,j]
    and set (i : int, j : int) (p : char) = arr.[i,j] <- p
  member this.count = mList.Length + wList.Length
  member this.neighbours pos =
    let delta = [(-1,1);(0,1);(1,1);(-1,0);(1,0);(-1,-1);(0,-1);(1,-1)]
    let getContent d =
      let di = fst pos + fst d
      let dj = snd pos + snd d
      if di < 0 || di > n-1 || dj < 0 || dj > n-1 then
        None
      else
        Some ((di,dj), arr.[di,dj])
    List.map getContent delta
  member this.anyEmptyNeighbour pos =
    let neighbours = this.neighbours pos
    let filter = Option.bind (fun e -> if snd e = ' ' then None else Some e) // Curry
    let shortened = List.choose (filter) neighbours
    if List.isEmpty shortened then None
    else shortened.[rnd.Next(shortened.Length)]
  member this.anyEmptyField () =
    let mutable i = rnd.Next (n-1)+1
    let mutable j = rnd.Next (n-1)+1
    while arr.[i,j] <> ' ' do
      i <- rnd.Next (n-1)+1
      j <- rnd.Next(n-1)+1
    (i,j)
  override this.ToString () =
    let mutable ret = ""
    for i = 0 to n-1 do
      for j = 0 to n-1 do
        ret <- ret + string arr.[i,j] + " "
      ret <- ret + "\n"
    ret + "\n"
  member this.allAction (mlsrc,mldest,wlsrc,wldest) =
    let rnd = Random ()
    if rnd.Next(1) > 0 then // fst list
      match mlsrc with
        m::rest -> m.action(); this.allAction (rest,m::mldest,wlsrc,wldest)
        | _ -> ([],m::mldest,wlsrc,wldest)
    else
      match wlsrc with
        w::rest -> w.action(); this.allAction (mlsrc,mldest,rest,w::wldest)
        | _ -> (mlsrc,mldest,[],w::wldest)


and animal (initialReproduction : int, env : environment) =
  let mutable _reproduction = initialReproduction
  let mutable _pos = None
  member this.position
    with get () = _pos
    and set aPos = _pos <- aPos
  member this.reproduction = _reproduction
  member this.tick () =
    _reproduction <- _reproduction - 1
  member this.resetHunger () =
    _reproduction <- initialReproduction
  member this.move () =
    let moveit src dest =
      env.[fst dst, snd dst] = env.[fst src, snd src]
      env.[fst src, snd src] = ' '
      dest
    let newPos = Option.bind env.anyEmptyNeighbour _pos
    _pos <- Option.bind (movit _pos) newPos

and moose (rep : int, env : environment) =
  inherit animal (rep, env)
  member this.action () = this.move ()

and wulf (initialHunger : int, rep : int, env : environment) =
  inherit animal (rep, env)
  let mutable _hunger = initialHunger
  member this.hunger = _hunger
  member this.tick () =
    _hunger <- _hunger - 1
  member this.eat () =
    _hunger <- initialHunger
  member this.action () = this.move ()

let e = 10
let u = 5
let n = 10
let fe = 20
let fu = 20
let s = 5
let T = 10
let isle = environment(n,e, fe, u, fu, s)

for t = 1 to T do
  printfn "%A" isle
  isle.allAction ()

