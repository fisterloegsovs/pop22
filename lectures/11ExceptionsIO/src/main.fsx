[<EntryPoint>]
let main (args: string array) : int =
  for a in args do printfn "%s" a
  0 // status code "ok"
