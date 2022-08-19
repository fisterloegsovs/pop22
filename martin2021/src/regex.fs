
open System.Text.RegularExpressions

let r2 = Regex "^[1-9][0-9]*$"

let res1 = if r2.IsMatch "2320" then "ok" else "err"

let res2 = if r2.IsMatch "23d20" then "err" else "ok"

do printfn "%s" res1
do printfn "%s" res2

let extract (r:Regex) (s:string) : string option =
  let m = r.Match s
  in if m.Success then Some (string(m.Groups.[1]))
     else None

let r = Regex "is ([1-9][0-9]*) years"
let text = "Hans is 34 years old"
do printfn "%A" (extract r text)
