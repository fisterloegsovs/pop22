let filename = "openFile.fsx"
let reader =
  try
    Some (System.IO.File.OpenText filename)
  with
    _ -> None

if reader.IsSome then
  while not(reader.Value.EndOfStream) do
    let c = reader.Value.Read ()
    printf "'%c', " (char c)
    
  reader.Value.Close ()
else
  printfn "File %A could not be read." filename
  
