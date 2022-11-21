module queue

let mutable (q: int[]) = [||]

let enqueue (e: int) : unit = q <- Array.append q [|e|]
let dequeue () : int option =
  match Array.length q with
    0 -> None
    | _ -> 
      let v = Array.head q
      q <- Array.tail q
      Some v
let isEmpty () : bool = Array.isEmpty q
let toString () : string =
  match Array.length q with
    0 -> "[||]"
    | 1 -> "[|"+(string q[0])+"|]"
    | _ ->
      (Array.fold (fun acc elm -> acc+", "+(string elm)) ("[|"+(string q[0])) q[1..])+"|]"
