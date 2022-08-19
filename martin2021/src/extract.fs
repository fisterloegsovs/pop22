open System.Text.RegularExpressions

let extract (r:Regex) (s:string) : string option =
  let m = r.Match s
  in if m.Success then Some (string(m.Groups.[1]))
     else None

let r = Regex "is ([1-9][0-9]*) years"
let text = "Hans is 34 years old"
do printfn "%A" (extract r text)
