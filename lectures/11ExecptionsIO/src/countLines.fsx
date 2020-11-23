[<EntryPoint>]
let main (args:string array) : int =
  let rec loop n (r:System.IO.StreamReader) =
    if r.EndOfStream then
      n
    else
      ignore(r.ReadLine())
      loop (n+1) r
  if Array.length args > 0 then
    let fp = System.IO.File.OpenText args.[0]
    printfn "%d" (loop 0 fp)
    0
  else
    printfn "Expects file name as argument"
    1
