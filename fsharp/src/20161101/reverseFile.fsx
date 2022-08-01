let rec readFile (stream : System.IO.StreamReader) =
  if not(stream.EndOfStream) then
    (stream.ReadLine ()) :: (readFile stream)
  else
    []

let rec writeFile (stream : System.IO.StreamWriter) text =
  match text with
  | (l : string) :: ls ->
    writeFile stream ls
    stream.WriteLine l
  | _ -> ()

let reverseString (s : string) =
  System.String(Array.rev (s.ToCharArray()))

let inputStream = System.IO.File.OpenText "reverseFile.fsx"
let text = readFile inputStream
let reverseText = List.map reverseString (List.rev text)
let outputStream = System.IO.File.CreateText "xsf.eliFesrever"
writeFile outputStream reverseText
outputStream.Close ()
printfn "%A" reverseText
