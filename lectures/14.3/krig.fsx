type bord() =
  let deltagere : spiller list = []
  member uddelKort () : unit = ()
  member spilOmgang () : bool

type spiller() =
  let stak = bunke()
  member afgivKort () = kort()
  member modtagKort (etKort : kort) = ()

type bunke() =
  let lst : kort list = []
  member afgivKort () = kort()
  member modtagKort (etKort : kort) = ()

type kort() =
  class end

type vaerdi() =
  class end
  
