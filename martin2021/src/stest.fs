// Problematic string concatenation
let rec loop i =
  if i < 1 then "" else loop (i-1) + i.ToString()

let len = String.length(loop 50000)

do printfn "Size of string with Numbers from 1 to 50000: %d" len
