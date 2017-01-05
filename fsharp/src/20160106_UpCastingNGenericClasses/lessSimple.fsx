type parent(v) =
  member val a = v

type child(v1, v2) = 
  inherit parent(v1)
  member val a = v1
  member val b = v2

let aParent = new parent(1.0)
let aChild = new child(2.0, 3.0)
let anotherChild = aChild :> parent

printfn "%A" [aParent.a; aChild.a; aChild.b; anotherChild.a]
