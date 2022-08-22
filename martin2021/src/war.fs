// File war.fs: Simulation of the war card game
// Copyright (c) 2014, Martin Elsman.
// MIT License.

let rand : int -> int =
  let rnd = System.Random()
  in fun n -> rnd.Next(0,n)   // draw a number in the range [0;n[

type card = int
type player = {hand: card list; table: card list}

let rec deal cs (h1,h2) =
  match cs with
    | [] -> ({hand=h1;table=[]}, {hand=h2;table=[]})
    | c1::c2::cs -> deal cs (c1::h1, c2::h2)
    | [c] -> deal [] (c::h1,h2)

let shuffle (cs:card list) : card list =
  let is = List.init (List.length cs) (fun _ -> rand 10000)
  let pairs = List.map2 (fun c i -> (c,i)) cs is
  let sorted = List.sortWith (fun (_,i1) (_,i2) -> compare i1 i2) pairs
  in List.map fst sorted

let color : card list = [1;2;3;4;5;6;7;8;9;10;11;12;13]
let deck = color @ color @ color @ color

let newdeck() = shuffle deck

let pp (cs:card list) = "[" + String.concat "," (List.map string cs) + "]"
let pr cs = printf "%s\n" (pp cs)

//do pr (newdeck())

let rec getCard (p:player) : (card*player) option =
  match p with
    | {hand=[];table=[]} -> None
    | {hand=c::hand;table=table} -> Some(c,{hand=hand; table=table})
    | {hand=[];table=table} -> getCard {hand=shuffle table; table=[]}

let addCards (p:player) cs : player =
  {hand= p.hand; table= cs @ p.table}

let rec game (n:int, pool:int list, p1:player, p2:player) : int * int =
  if n >= 1000 then (0,1000)
  else match (getCard p1, getCard p2) with
         | (Some(c1,p1),Some(c2,p2)) ->
           if c1 > c2 then game (n+1,[],addCards p1 ([c1;c2]@pool),p2)
           else if c2 > c1 then game (n+1,[],p1,addCards p2 ([c1;c2]@pool))
           else (match (getCard p1, getCard p2) with
                | (Some(c1,p1),Some(c2,p2)) -> game (n+2, [c1;c2]@pool, p1, p2)
                | (Some _, None) -> (1,n)
                | (None, Some _) -> (2,n)
                | (None, None) -> (0,n))     // pool may be filled - it's a tie!
         | (Some _, None) -> (1,n)
         | (None, Some _) -> (2,n)
         | (None, None) -> (0,n)             // pool may be filled - it's a tie!

let count p xs = List.fold (fun n x -> if p x then n+1 else n) 0 xs

let pr_gs gs =
  let win1 = count (fun (w,_) -> w=1) gs
  let win2 = count (fun (w,_) -> w=2) gs
  let ties = count (fun (w,_) -> w=0) gs
  let turns = List.fold (fun a (_,n) -> a+n) 0 gs / List.length gs
  in printf "Player 1 wins: %d, Player 2 wins: %d, Ties: %d, Avg turns: %d\n" win1 win2 ties turns

let runGame() =
  let (p1,p2) = deal (newdeck()) ([],[])
  in game (0, [], p1, p2)

let rec runGames n acc =
  match n with
    | 0 -> pr_gs acc
    | _ -> runGames (n-1) (runGame()::acc)

do runGames 100000 []
