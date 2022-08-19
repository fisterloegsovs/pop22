open System.Net
open System
open System.IO
open System.Text.RegularExpressions

// Utility for extracting value from a string
let extract (r:Regex) (s:string) : string option =
  let m = r.Match s
  in if m.Success then Some (string(m.Groups.[1]))
     else None

// Fetch the contents of a web page
let fetchUrl url =
  let req = WebRequest.Create(Uri(url))
  use resp = req.GetResponse()
  use stream = resp.GetResponseStream()
  use reader = new IO.StreamReader(stream)
  in reader.ReadToEnd()

// Find FX rate for a particular currency
let findfx cur s =
  let floatregex = "([1-9][0-9]*,[0-9]+)"
  let regex = cur + ".*\\n.*>" + floatregex + "<"
  in match extract (Regex regex) s with
      | Some v -> v
      | None -> "error"

do printf "Enter currency (CUR): "
let cur = System.Console.ReadLine()
let cur_regex = Regex "USD|EUR|CHF|SEK|NOK|GBP|AUD|JPY|CAD"

if cur_regex.IsMatch cur then
  let s = fetchUrl "https://www.nationalbanken.dk/valutakurser"
  let fx = findfx cur s
  do printfn "%sDKK=%s" cur fx
else
  do printfn "Currency '%s' not supported" cur
