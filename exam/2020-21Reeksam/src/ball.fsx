type ball(p) =
  let mutable _pos = (0,0)
  member this.pos 
    with get () = p
    and set (new_p) = _pos <- new_p
//
let b1 = ball((1,1))
let b2 = ball((0,0))
printfn "b1.pos = %A, b2.pos = %A" b1.pos b2.pos
