module Vector
type Vector<'t> =
  | V of 't * 't
  static member inline( +. ) : Vector< ^a> * Vector< ^b> -> Vector< ^c>
    when ( ^a or  ^b) : (static member ( + ) :  ^a *  ^b ->  ^c)
val make : x:'t * y:'t -> Vector<'t>
val coord : Vector<'t> -> 't * 't
