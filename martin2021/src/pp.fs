let rec padl n s =
  if String.length s > n then s
  else padl (n-1) (" " + s)

let pp n = String.concat "\n"
           << List.map (String.concat " "
                        << List.map (padl n << string))

do printfn "%s" (pp 3 [[1;2;5];[12;3;25];[7;32;1]])
