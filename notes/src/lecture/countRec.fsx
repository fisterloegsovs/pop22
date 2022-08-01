let countRec n =
  let rec countHelper n =
    if n >= 0 then
      (countHelper (n-1)) + " " + (string n)
    else
      ""

  printfn "%s" (countHelper n)

countRec 3
countRec 10
countRec 0
countRec -1
  
