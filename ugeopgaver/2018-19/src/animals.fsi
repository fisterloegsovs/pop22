module animals
type symbol = char
type position = int * int
type animal =
  class
    new : symb:symbol * repLen:int -> animal
    override ToString : unit -> string
    member symbol : symbol
    member position : position option
    member position : position option with set
    member reproduction : int
    member resetReproduction : unit -> unit
    member updateReproduction : unit -> unit
  end
type moose =
  class
    inherit animal
    new : repLen:int -> moose
    member updateReproduction : unit -> moose option
    member tick : unit -> moose option
  end
type wulf =
  class
    inherit animal
    new : repLen:int * hungLen:int -> wulf
    member updateReproduction : unit -> wulf option
    member hunger : int
    member resetHunger : unit -> unit
    member updateHunger : unit -> unit
    member tick : unit -> wulf option
  end
type board =
  {width: int;
   mutable moose: moose list;
   mutable wulves: wulf list;}
type environment =
  class
    new : boardWidth:int * NMooses:int * mooseRepLen:int * NWolves:int *
          wolvesRepLen:int * wolvesHungLen:int -> environment
    override ToString : unit -> string
    member board : board
    member count : int
    member size : int
    member tick : unit -> unit
  end
