let rec reverseString (str : string) =
  let l = String.length str
  if l > 0 then
    (string str.[l-1]) + (reverseString str.[0..l-2])
  else
    ""

let rec reverseLines (inStream : System.IO.StreamReader) (outStream : System.IO.StreamWriter) =
  while not(inStream.EndOfStream) do
    let str = inStream.ReadLine ()
    outStream.WriteLine (reverseString str)

let inputStream = System.IO.File.OpenText "reverseFile.fsx"
let outputStream = System.IO.File.CreateText "xsf.eliFesrever"
reverseLines inputStream outputStream
inputStream.Close ()
outputStream.Close ()
