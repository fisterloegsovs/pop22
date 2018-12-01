module animals
/// A symbol to print an animal on a board
type symbol = char
/// A position on a board
type position = int * int
/// Base class for all animals
type animal =
  class
    /// Create a new animal represented with symbol symb and which reproduces every repLen ticks.
    new : symb:symbol * repLen:int -> animal
    /// The symbol as string for printing.
    override ToString : unit -> string
    /// The symbol representing this animal.
    member symbol : symbol
    /// Get the position of this animal. If position is None, then the animal is not on the board.
    member position : position option
    /// Set the position of this animal.
    member position : position option with set
    /// Get the reproduction counter in ticks. Starts as repLen and is counted down with every tick.
    member reproduction : int
    /// Set the reproduction counter to repLen.
    member resetReproduction : unit -> unit
    /// Reduce the reproduction counter by a tick
    member updateReproduction : unit -> unit
  end
/// A moose
type moose =
  class
    inherit animal
    /// Create a moose with symbol 'm' and which reproduces every repLen ticks.
    new : repLen:int -> moose
    /// Reduce the reproduction counter by a tick. If repLen is 0 then calf is returned and the counter is reset to repLen.
    member updateReproduction : unit -> moose option
    /// Perform a tick for this moose, i.e., call updateReproduction
    member tick : unit -> moose option
  end
/// A wolf
type wolf =
  class
    inherit animal
    /// Create a moose with symbol 'w', which reproduces every repLen ticks, and which has hungLen ticks to live in
    new : repLen:int * hungLen:int -> wolf
    /// Reduce the reproduction counter by a tick. If repLen is 0 then a new wolf is returned and the counter is reset to repLen.
    member updateReproduction : unit -> wolf option
    /// Get the hunger counter in ticks. Starts as hungLen and is counted down with every tick.
    member hunger : int
    /// Set the hunger counter to hungLen.
    member resetHunger : unit -> unit
    /// Reduce the hunger counter by a tick. If hunger reaches 0, then the wolf is removed from the board
    member updateHunger : unit -> unit
    /// Perform a tick for this wolf, i.e., call updateReproduction and updateHunger and possibly returns cub.
    member tick : unit -> wolf option
  end
/// A square board with length width. The board is implicitly represented by its width and the coordinates in the animals
type board =
  {width: int;
   mutable moose: moose list;
   mutable wolves: wolf list;}
/// An environment. Animals that have no position are considered dead
type environment =
  class
    /// Create a new environment of width boardWidth, NMooses moose and NWolves. The moose have reproduction length mooseRepLen and the wolves have reproduction length wolvesRepLen and hunger length wolvesHungLen
    new : boardWidth:int * NMooses:int * mooseRepLen:int * NWolves:int *
          wolvesRepLen:int * wolvesHungLen:int -> environment
    /// A board as a matrix of symbols for moose and wolves
    override ToString : unit -> string
    /// The board
    member board : board
    /// The number of animals on the board
    member count : int
    /// The positions on the board
    member size : int
    /// Perform a tick by performing all animal's ticks in random order. Animals perform the following actions: Calves and cubs are added if there is room in a neighbouring position. Wolves eat a random Moose in a neighbouring position. If animals do not give birth, eat or are eaten, then they move to an available neighbouring position.
    member tick : unit -> unit
  end
