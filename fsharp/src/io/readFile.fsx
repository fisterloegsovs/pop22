let rec documentFileName () =
  System.Console.Write("Filename: ")
  let filename = System.Console.ReadLine()
  if not(System.IO.File.Exists filename) then
    System.Console.WriteLine("File does not exist")
    documentFileName ()
  else filename

let rec printFile (reader : System.IO.StreamReader) =
  if not(reader.EndOfStream) then
    let line = reader.ReadLine ()
    printfn "%s" line
    printFile reader
          

let x = System.IO.Directory.GetFiles(".")
printfn "Directory contains: %A" x

let filename = documentFileName ()

let reader = System.IO.File.OpenText filename
printFile reader
