let id = 3
let message = 
  match id with
    1 -> "You have selected number 1"
    | 2 -> "You have selected number 2"
    | _ -> "Unknown id"
    | 3 -> "You have selected number 3"

printfn "Message: %A" message
