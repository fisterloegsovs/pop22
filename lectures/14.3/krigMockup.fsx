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
  member this.spilOmgang () = false

