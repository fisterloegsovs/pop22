type shish_kebab =
      | Skewer
      | Onion of shish_kebab
      | Lamb of shish_kebab
      | Tomato of shish_kebab
let empty (kebab : shish_kebab) : bool =
  match kebab with
    | Skewer -> true
    | _      -> false
//
let kebab1 = Onion(Lamb(Onion(Skewer)))
let kebab2 = Tomato(Tomato(Onion(Onion(Tomato(Skewer)))))
printfn "Er der fyld på kebab1? %b" (empty kebab1 |> not) 
