let rec documentFileName (txt : string) =
  System.Console.Write (txt)
  let filename = System.Console.ReadLine ()
  if not (System.IO.File.Exists filename) then
    System.Console.WriteLine ("File does not exist")
    documentFileName txt
  else filename

let rec readFile (stream : System.IO.StreamReader) =
  if not(stream.EndOfStream) then
    ( stream.ReadLine () ) :: ( readFile stream )
  else
    []

let rec writeFile (stream : System.IO.StreamWriter) = function
  | l : string :: _ -> stream.WriteLine l 
  | [] -> ()

let x = System.IO.Directory.GetFiles(".")
printfn "Directory contains: %A" x

let inputFilename = documentFileName "Type input filename:"
System.Console.Write "Type output filename:"
let outputFilename = System.Console.ReadLine ()
  
use inputStream = System.IO.File.OpenText inputFilename
let linesInFile = readFile inputStream

use outputStream = System.IO.File.CreateText outputFilename
