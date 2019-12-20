type vaerdi() =
  class end

type kort() =
  let v = vaerdi ()
  member this.sammenlign (etKort : kort) = false

type bunke() =
  let lst : kort list = []
  member this.afgivKort () = kort()
  member this.modtagKort (etKort : kort) = ()

type spiller() =
  let stak = bunke()
  member this.afgivKort () = kort()
  member this.modtagKort (etKort : kort) = ()

type bord() =
  let deltagere : spiller list = []
  member this.uddelKort () : unit = ()
  member this.spilOmgang () = true

// Main loop
let b = bord()
let mutable i = 0
while b.spilOmgang () do
  i <- i + 1;
  printfn "Omgang %d" i
