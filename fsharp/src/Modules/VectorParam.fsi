module Vector
type Vector<'t>
val add : Vector< 't> -> Vector< 't> -> Vector< 't>
    when ( 't or  't) : (static member ( + ) :  't *  't ->  't)
val make : 't * 't -> Vector<'t>
val coord : Vector<'t> -> 't * 't
