open textAnalysis

[<EntryPoint >]
let main (args : string []) : int =
  printfn "args: %A" args
  // fct <filename> <seed> <m> [v]
  if args.Length < 3 || args.Length > 4 then
    printfn "fct <filename> <seed> <m> [v]"
    1
  else
    let fname = args.[0]
    let seed = args.[1].ToCharArray() |> Array.toList
    let n = List.length seed
    let m = int args.[2]
    let verbose = args.Length = 4
    printfn "%A %A %A %A %A" fname n seed m verbose
        
    let textConv = readfile fname |> convertText
    if verbose then
      printfn "Original text:\n%s" textConv
    let textList = textConv.ToCharArray() |> Array.toList
    let hist = textList |> List.countBy id |> List.sortBy fst
    if verbose then
      printfn "Histogram:\n%A" hist
    let textNGram = ngram textList n
    if verbose then
      printfn "nGram:\n%A" textNGram
    let newText = randomText textNGram hist seed m |> List.fold (fun acc elm -> acc+(string elm)) ""

    printfn "%A" newText
    0
