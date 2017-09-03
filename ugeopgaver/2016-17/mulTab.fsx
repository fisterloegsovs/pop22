let mulTab n =
  let mutable str = "    "
  for i = 1 to 10 do
    str <- str + sprintf "%4d" i
  str <- str + "\n"
  for i = 1 to n do
    str <- str + sprintf "%4d" i
    for j = 1 to 10 do
      str <- str + sprintf "%4d" (i * j)
    str <- str + "\n"
  str
printf "%s" (mulTab 3);;
