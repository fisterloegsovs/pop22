open System.Diagnostics

let lst = [1UL..1000000UL]
let timer = Stopwatch()
let N = 3;

for i = 1 to N do
  timer.Reset()
  timer.Start()
  let af = List.fold (fun acc elm -> acc + elm) 0UL lst
  printfn "fold sum: %A" af
  timer.Stop()
  printfn "fold took: %d ms" timer.ElapsedMilliseconds

for i = 1 to N do
  timer.Reset()
  timer.Start()
  let ab = List.foldBack (fun elm acc -> acc + elm) lst 0UL
  printfn "foldback sum: %A" ab
  timer.Stop()
  printfn "foldback took: %d ms" timer.ElapsedMilliseconds

