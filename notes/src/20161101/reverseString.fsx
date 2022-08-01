let rec reverseString (str : string) =
  let l = String.length str
  if l > 0 then
    (string str.[l-1]) + (reverseString str.[0..l-2])
  else
    ""

let str = "hejsa"
printfn "%A <-> %A" str (reverseString str)
