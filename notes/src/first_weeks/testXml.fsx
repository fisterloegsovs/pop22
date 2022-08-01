// How to compile: fsharpc --standalone -r Fsharp.Data/bin/FSharp.Data.dll testXml.fsx
// If in interactive mode, then use: #r "Fsharp.Data/bin/FSharp.Data.dll"
open FSharp.Data

let authors = """
  <authors topic="Philosophy of Mathematics">
    <author name="Bertrand Russell" />
    <author name="Ludwig Wittgenstein" born="1889" />
    <author name="Alfred North Whitehead" died="1947" />
  </authors> """

type Authors = XmlProvider<"Writers.xml">
let topic = Authors.Parse(authors)

printfn "%s" topic.Topic
for author in topic.Authors do
  printf " - %s" author.Name 
  author.Born |> Option.iter (printf " (%d)")
  printfn ""

type doc = XmlProvider<"docExample.xml">

let anExample = """
<doc>
  <assembly><name>Vector</name></assembly>
  <members>
    <member name="T:Vector.Vector">
      <summary>
	A demonstration of defining a module from H &amp; R, Functional Programming Using F#. Note: Bad style, better use augmented types.
	
	How to compile:
	&lt;code&gt;
	fsharpc --doc:Vector.xml -a Vector.fsi Vector.fs
	fsharpc --doc:testVector.xml -r Vector.dll testVector.fsx
	&lt;/code&gt;
	
	Author: Jon Sporring.
	Date: 2015/10/27
	A 2 dimensional vector type, whose elements are floats.
      </summary>
    </member>
    <member name="M:Vector.coord(Vector.Vector)">
      <summary>
	Get coordinates
      </summary>
    </member>
    <member name="M:Vector.make(System.Double,System.Double)">
      <summary>
	Make vector
      </summary>
    </member>
  </members>
</doc>"""

let aDoc = doc.Parse(anExample)
for m in aDoc.Members do
  printfn "Name: %s" m.Name 
  printfn "Symmary: %s" m.Summary 
