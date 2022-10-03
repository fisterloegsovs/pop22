let i = 10u
let rec countDown () =
  match i with
    0u -> ()
    | _ ->
      printfn "%A" i
      let i = i - 1u 
      countDown ()

countDown ()
