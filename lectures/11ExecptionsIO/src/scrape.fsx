open System.Net
open System
open System.IO
let fetchUrl (url:string) : string =
  let req = WebRequest.Create(Uri(url))
  use resp = req.GetResponse()
  use stream = resp.GetResponseStream()
  use reader = new IO.StreamReader(stream)
  reader.ReadToEnd()

let str = fetchUrl "http://www.valutakurser.dk"
printfn "%s" str
