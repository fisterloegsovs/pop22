// Compile with
//   $ fsharpc --nologo -r pcomb.dll tokens.fs
//
// Run with
//   $ echo 'hello world, great' | mono tokens.exe ',.'
//
// Assumes
//   $ fsharpc -a pcomb.fsi pcomb.fs

open PComb

[<EntryPoint>]
let main (args: string array) : int =
  try
    let cs = args.[0]
    let line = System.Console.ReadLine()
    let ts = tokenize (" "+cs) line |> elimWS
    do List.iter (fun t -> printfn "%s" t) ts
    0
  with _ ->
    do printfn "Usage: mono tokens.exe 'split chars'"
    -1
