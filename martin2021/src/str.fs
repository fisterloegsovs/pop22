let rec loop i =
  if i < 1 then "" else loop (i-1) + i.ToString()

let y = loop 50000
