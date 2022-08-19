
open PComb

[<EntryPoint>]
let main ( args : string array ) : int =
  try
    let cs = args.[0]
    let line = System.Console.ReadLine()
    let ts : string list = tokenize (" " + cs) line
                           |> elimWS
    do List.iter (fun t -> printfn "%s" t) ts
    0
  with _ ->
    do printfn "Usage: mono tokens.exe 'split chars'"
    -1
