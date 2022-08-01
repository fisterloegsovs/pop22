// How to compile: fsharpc --standalone -r Fsharp.Data/bin/FSharp.Data.dll testXml.fsx
// If in interactive mode, then use: #r "Fsharp.Data/bin/FSharp.Data.dll"

// First we define the type by an example.
type Authors = FSharp.Data.XmlProvider<"writersSample.xml">

// Then we define declare a string to be parsed
let filename = "scienceWriters.xml"
let utf8 = System.IO.File.ReadAllText(filename, System.Text.Encoding.UTF8)

let topic = Authors.Parse(utf8)

printfn "%s" topic.Topic
for author in topic.Authors do
  printf " - %s" author.Name 
  author.Born |> Option.iter (printf " (%d)")
  printfn ""
