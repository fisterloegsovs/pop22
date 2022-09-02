let rec countDown n =
  printfn "%A" n
  match n with
    0 -> ()
    | _ -> countDown (n-1)

countDown 5
