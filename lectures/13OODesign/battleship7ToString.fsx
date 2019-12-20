/// A coordinate type is a row-column pair
type coordinate = int*int

/// A ship has a size and keeps track of the number of  hits, it has
/// received.
type ship(sz : int) =
  let mutable _noHits = 0

  /// the size of a ship
  member this.size = sz
  /// Increase the number of hits a ship has taken
  member this.hit () = _noHits <- _noHits + 1
  /// Check if a ship has taken more hits than its size
  member this.isSunk () = _noHits >= sz
  
/// A field's state may have been shot at and may be occupied by a
/// ship
type State = bool * (ship option)

type board (sz : int*int) =
  // A board is a 2d array of fields, each with a state of having been
  // shot at or not, and an optional reference to a ship object.
  let (_rows, _cols) = sz
  let _fields = Array2D.create _rows _cols ((false, None) : State)

  /// Produce a string-representation of the board as a table with the
  /// sympols " " - no shot taken and no ship, "s" - no shot take and
  /// a ship present, "m" - shot take but not ship present, "h" shot
  /// take and ship hit.
  override this.ToString () =
    let digits = max _rows _cols |> float |> log10 |> ceil |> int
    let mutable str = (String.replicate (digits+1) " ")
    for j = 1 to _cols do
      str <- str + (sprintf "%*d " digits j)
    str <- str + "\n"
    for i = 1 to _rows do
      str <- str + (sprintf "%*d " digits i)
      for j = 1 to _cols do
        let sym =
          match _fields.[i-1,j-1] with
            (true, None ) -> "m"
            | (true, Some s) -> "h"
            | (false, Some s) -> "s"
            | _ -> " "
        str <- str + (sprintf "%*s " digits sym)
      str <- str + "\n"
    str.[..str.Length-2]

let b = board (10,10)
printfn "My board:\n%A" b
