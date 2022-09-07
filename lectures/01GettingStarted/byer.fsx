let rec dialog () =
  printfn "Antal indbyggere: Indtast et bynavn."
  let by = System.Console.ReadLine ()
  let antal = 
    match by with
      "København" -> 1345562
      | "Aarhus" -> 285273
      | "Odense" -> 180863
      | "Aalborg" -> 119862
      | _ -> -1
  match antal with
    -1 -> 
      printfn "Kender ikke %A" by
      ()
    | _ -> 
      printfn "I %A bor der %A mennesker (wikipedia, 2022)" by antal
      dialog ()
dialog ()