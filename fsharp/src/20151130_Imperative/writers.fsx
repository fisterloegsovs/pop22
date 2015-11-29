// How to compile: fsharpc --standalone -r Fsharp.Data/bin/FSharp.Data.dll testXml.fsx
// If in interactive mode, then use: #r "Fsharp.Data/bin/FSharp.Data.dll"

// First we define the type by an example.
type Authors = FSharp.Data.XmlProvider< """
<authors topic="A topic">
  <author name="A name" born="1900" />
  <author name="Another name" />
</authors>  """>

// Then we define declare a string to be parsed
let authors = """
  <authors topic="Philosophy of Mathematics">
    <author name="Bertrand Russell" />
    <author name="Ludwig Wittgenstein" born="1889" />
    <author name="Alfred North Whitehead" died="1947" />
  </authors> """

// parse the file
let topic = Authors.Parse(authors)

// Print it
printfn "%s" topic.Topic
for author in topic.Authors do
  printf " - %s" author.Name 
  author.Born |> Option.iter (printf " (%d)")
  printfn ""
