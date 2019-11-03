open textAnalysis
//open System.Collections.Generic

let ngram (lst : 'a list) (n : int) : Map<'a list, ('a * int) list> =
  let rec populate (dict : Map<'a list, 'a list>) (lst : 'a list) (n : int) : Map<'a list, 'a list> =
    if lst.Length > n then
      let key = lst.[0..(n-1)]
      let obs = lst.[n]
      let obsLst = dict.TryFind key
      let dictNew =
        match obsLst with
          Some aLst -> (dict.Remove key).Add (key, obs::aLst)
          | None -> dict.Add (key, [obs])
      populate dictNew lst.[1..] n
    else
      dict

  let dict = Map.empty<'a list, 'a list>
  let aMap = populate dict lst n
  Map.map (fun key lst -> lst |> List.countBy id |> List.sortBy fst) aMap

let text = readfile "littleClausAndBigClaus.txt"
let textConv = convertText text

//let textConv = "der var en gang var der"
let textList = textConv.ToCharArray() |> List.ofArray
let textNGram = ngram textList 5
printfn "%s\n%A" textConv textNGram
(*
   printfn "A histogram:\n %A" (List.zip alphabet textHist)
let textRanStr = randomString textHist textConv.Length
//printfn "A random string: %s" textRanStr
let newTextHist = histogram textRanStr
printfn "Resulting histogram:\n %A" (List.zip alphabet newTextHist)
printfn "Sum of squared differences is: %g" (compareHist textHist newTextHist)
*)
