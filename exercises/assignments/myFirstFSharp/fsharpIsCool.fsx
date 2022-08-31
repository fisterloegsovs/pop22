let rec getName () =
  printfn "Please enter the name of a programming language:"
  let a = System.Console.ReadLine ()
  match a with
    "quit" ->
      ()
    | "fsharp" ->
      printfn "Fsharp is cool"
      getName ()
    | _ ->
      printfn "I don't know %A" a
      getName ()
      
let b = getName ()
