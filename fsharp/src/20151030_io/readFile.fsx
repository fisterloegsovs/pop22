/// A demonstration of reading a text file.
///
/// How to compile: <code>fsharpc --doc:readFile.xml readFile.fsx</code>
///
/// Author: Jon Sporring.
/// Date: 2015/10/27

/// <summary>Dialogue for asking the user to input the name of an existing file.</summary>
/// <returns>A valid filename.</returns>
let rec documentFileName () =
  System.Console.Write("Filename: ")
  let filename = System.Console.ReadLine()
  if not(System.IO.File.Exists filename) then
    System.Console.WriteLine("File does not exist")
    documentFileName ()
  else filename

/// <summary>Print the contents of a text file to screen.</summary>
/// <param name="reader">An open stream.</param>
let rec printFile (reader : System.IO.StreamReader) =
  if not(reader.EndOfStream) then
    let line = reader.ReadLine ()
    printfn "%s" line
    printFile reader
          
/// Print the contents of the current directory and ask the user for a filename, and then print its contents to screen.
let x = System.IO.Directory.GetFiles(".")
printfn "Directory contains: %A" x

let filename = documentFileName ()

let reader = System.IO.File.OpenText filename
printFile reader
