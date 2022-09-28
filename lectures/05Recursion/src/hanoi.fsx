type piece = int
type peg = piece list
type board = peg list

let move (src: int) (tgt: int) (brd: board) =
  let pc = brd[src].Head
  let newSrc = brd[src].Tail
  let newTgt =  pc :: brd[tgt]
  let rec helper (n: int) (p: board) : board =
    match n with
      i when i >= 3 -> []
      | i when i = src -> newSrc :: helper (n+1) p.Tail
      | i when i = tgt -> newTgt :: helper (n+1) p.Tail
      | _ -> p.Head :: helper (n+1) p.Tail
  helper 0 brd

let rec hanoi (n: int) (src: int) (aux: int) (tgt: int) (brd: board) =
  if n = 0 then brd
  else
    let newBrd = 
      brd 
      |> hanoi (n-1) src tgt aux
      |> move src tgt
    printfn "%A" newBrd
    hanoi (n-1) aux src tgt newBrd

// hanoi: n: int -> src: int -> aux: int -> tgt: int -> board -> board
let brd: board = [[1;2;3];[];[]]
printfn "%A" brd
hanoi 3 0 1 2 brd // Move 3 pieces from peg 1 to 3 with the help of peg 2
