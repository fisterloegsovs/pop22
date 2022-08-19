type 'a m = string -> 'a option

let empty () : 'a m = fun _ -> None

let insert s v (m:'a m) : 'a m =
  fun x -> if x = s then Some v
           else m x

let lookup (m:'a m) s : 'a option = m s

let m = insert "b" 8 (insert "a" 5 (empty())) // val m : int m

do printfn "%A" (lookup m "c")  // val it : int option = None

do printfn "%A" (lookup m "a")  // val it : int option = Some 5
