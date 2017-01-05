type parent() =
  member val a = 1

type child() = 
  inherit parent()
  member val a = 2
  member val b = 3

let aParent = new parent()
let aChild = new child()
let anotherChild = aChild :> parent

printfn "%A" [aParent.a; aChild.a; aChild.b; anotherChild.a; anotherChild.b]
