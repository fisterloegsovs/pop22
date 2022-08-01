exception DontDoIt of string

let div a b =
  if b <> 0 then
    a/b
  else
    raise (DontDoIt "I said no")
    
let aList = [-1;0;1]
let bList = [-1;0;1]

for b in bList do
  for a in aList do
    let str =
      try
        sprintf "%d/%d = %d" a b (div a b)
      with
        _ -> "I said no"
    printfn "%s" str
