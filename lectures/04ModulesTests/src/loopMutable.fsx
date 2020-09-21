let stringDoubles n =
  let mutable str = ""
  for i = 1 to n do
    str <- str + " " + string (2*i)
  str.[1..]

printfn "%s" (stringDoubles 3)
