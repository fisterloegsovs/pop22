let filename = "openFile.fsx"
let reader =
  try
    Some (System.IO.File.Open (filename, System.IO.FileMode.Open))
  with
    _ -> None

if reader.IsSome then
  let mutable charLeft = reader.Value.Length
  while charLeft > -10L do
    let c = reader.Value.ReadByte ()
    printf "'%d', " c
    charLeft <- charLeft - 1L
    
  reader.Value.Close ()
else
  printfn "File %A could not be read." filename
  
