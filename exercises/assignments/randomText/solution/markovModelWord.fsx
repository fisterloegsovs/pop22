open textAnalysis

[<EntryPoint >]
let main (args : string []) : int =
  // fct <filename> <seed> <m> [v]
  if args.Length < 3 || args.Length > 4 then
    printfn "fct <filename> <seed> <m> [v]"
    1
  else
    let fname = args.[0]
    let seed = (args.[1] |> convertText).Split ' ' |> Array.filter (fun elm -> elm.Length > 0) |> Array.toList 
    let n = List.length seed
    let m = int args.[2]
    let verbose = if args.Length < 4 then 0 else int args.[3]
    if verbose > 1 then
      printfn "%A %A %A %A %A" fname n seed m verbose

    let textConv = readfile fname |> convertText
    if verbose > 1 then
      printfn "Original text:\n%s" textConv
    let textList = textConv.Split ' ' |> Array.toList 
    let hist = textList |> List.countBy id |> List.sortBy fst
    if verbose > 1 then
      printfn "Histogram:\n%A" hist
    let textNGram = ngram textList n
    if verbose > 1 then
      printfn "nGram:\n%A" textNGram
    let newText = randomText textNGram hist seed m (verbose > 0) |> String.concat " "

    printfn "%A" newText
    0
