module cyclicQueue

type Value = int
let mutable first : int option = None
let mutable last : int option = None
let mutable q : Value option[] = [||]

let create (n: int) : unit = 
  q <- Array.create n None
  first <- None
  last <- None
  
let enqueue (e: Value) : bool = 
  match first, last with
    Some i, Some j ->
      let t = (j+1) % q.Length      
      if t = i then 
        false
      else
        last <- Some t
        q[t] <- Some e
        true
    | _, _ -> 
      first <- Some 0
      last <- Some 0
      q[0] <- Some e
      true

let dequeue () : Value option =
  match first, last with
    Some i, Some j when i = j ->
      first <- None
      last <- None
      q[i]
    | Some i, Some j ->
      first <- Some ((i+1) % q.Length)
      q[i]
    | _, _ -> None

let isEmpty () : bool = first = None

let length () : int =
  match first, last with
    Some i, Some j when i < j ->
      j - i + 1
    | Some i, Some j ->
      q.Length + j - i + 1
    | _, _ -> 0

let toString () : string =
  match first, last with
    Some i, Some j ->
      let mutable str = string q[i].Value
      let mutable t = i
      while t <> j do
        t <- (t+1) % q.Length
        str <- str + ", " + (string q[t].Value)
      str
    | _, _ -> ""
