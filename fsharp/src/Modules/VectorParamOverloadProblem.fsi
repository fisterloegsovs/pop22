module Vector
/// Parametrised types have a problem with build in operators, since they tend to default to int in the compiler. Inline functions and static typing bypasses this, but inline functions are not accessible outside modules. Hence, this solution is useless!
type Vector<'t> =
  | V of 't * 't
  static member inline ( +. ) : Vector< ^a> * Vector< ^b> -> Vector< ^c>
    when ( ^a or  ^b) : (static member ( + ) :  ^a *  ^b ->  ^c)
val make : x:'t * y:'t -> Vector<'t>
val coord : Vector<'t> -> 't * 't
