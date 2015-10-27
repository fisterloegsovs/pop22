/// A demonstration of reading and writing text files.
///
/// How to compile: <c>fsharpc --doc:reverseFile.xml reverseFile.fsx</c>
///
/// Author: Jon Sporring.
/// Date: 2015/10/27

/// <summary>Dialogue for asking the user to input the name of an existing file.</summary>
/// <returns>A valid filename.</returns>
let rec documentFileName (txt : string) =
  System.Console.Write (txt)
  let filename = System.Console.ReadLine ()
  if not (System.IO.File.Exists filename) then
    System.Console.WriteLine ("File does not exist")
    documentFileName txt
  else filename

/// <summary>Read the contents of a text file line by line into a list.</summary>
/// <param name="stream">An open stream.</param>
/// <returns>The list of text lines.</returns>
let rec readFile (stream : System.IO.StreamReader) =
  if not(stream.EndOfStream) then
    ( stream.ReadLine () ) :: ( readFile stream )
  else
    []

/// <summary>Write a list of strings to a file.</summary>
/// <param name="stream">An open stream.</param>
/// <returns>Unit.</returns>
let rec writeFile (stream : System.IO.StreamWriter) = function
  | (l : string) :: ls -> stream.WriteLine l; writeFile stream ls
  | _ -> ()

/// <summary>Reverse a string.</summary>
/// <param name="s">Any string</param>
/// <returns>A string which is the reverse of s</returns>
let reverseString (s : string) = System.String(Array.rev (s.ToCharArray()))

/// <summary>Write a list of strings to a file in reverse order and where each string is reversed.</summary>
/// <param name="stream">An open stream.</param>
/// <param name="unnamed">A list of strings.</param>
/// <returns>Unit.</returns>
let rec sillyWriteFile (stream : System.IO.StreamWriter) = function
  | (l : string) :: ls ->
    stream.WriteLine (reverseString l)
    sillyWriteFile stream ls
  | _ -> ()

/// Print the contents of the current directory and ask the user for an input filename and an output filename, and then writes the content of the input file to the output file, in reverse order.
let x = System.IO.Directory.GetFiles(".")
printfn "Directory contains: %A" x

let inputFilename = documentFileName "Type input filename:"
System.Console.Write "Type output filename:"
let outputFilename = System.Console.ReadLine ()
  
let inputStream = System.IO.File.OpenText inputFilename
let linesInFile = readFile inputStream

let outputStream = System.IO.File.CreateText outputFilename
sillyWriteFile outputStream (List.rev linesInFile)
outputStream.Close () // Remember to close, otherwise the file might be empty!
