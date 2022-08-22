open System.Net
open System
open System.IO
open System.Text.RegularExpressions

// Fetch the contents of a web page
let fetchUrl url =
  let req = WebRequest.Create(Uri(url))
  use resp = req.GetResponse()
  use stream = resp.GetResponseStream()
  use reader = new IO.StreamReader(stream)
  in reader.ReadToEnd()

let s = fetchUrl "http://www.valutakurser.dk"
do printfn "%s" s
