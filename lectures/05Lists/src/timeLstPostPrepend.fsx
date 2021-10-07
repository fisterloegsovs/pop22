open System.Diagnostics

let timer = Stopwatch()
let N = 3;
let M = 10000;

for i = 1 to N do
  timer.Reset()
  timer.Start()
  let mutable lst = []
  for i = 1 to M do
      lst <- i::lst
  printfn "lst length: %A" lst.Length
  timer.Stop()
  printfn "prepend took: %d ms" timer.ElapsedMilliseconds

for i = 1 to N do
  timer.Reset()
  timer.Start()
  let mutable lst = []
  for i = 1 to M do
      lst <- lst @ [i]
  printfn "lst length: %A" lst.Length
  timer.Stop()
  printfn "postpend took: %d ms" timer.ElapsedMilliseconds

