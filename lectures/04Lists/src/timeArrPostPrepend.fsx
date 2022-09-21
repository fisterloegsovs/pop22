open System.Diagnostics

let timer = Stopwatch()
let N = 3;
let M = 100000;

for i = 1 to N do
  timer.Reset()
  timer.Start()
  let mutable arr = [||]
  for i = 1 to M do
      arr <- Array.append [|i|] arr
  printfn "arr length: %A" arr.Length
  timer.Stop()
  printfn "prepend took: %d ms" timer.ElapsedMilliseconds

for i = 1 to N do
  timer.Reset()
  timer.Start()
  let mutable arr = [||]
  for i = 1 to M do
      arr <- Array.append arr [|i|] 
  printfn "arr length: %A" arr.Length
  timer.Stop()
  printfn "postpend took: %d ms" timer.ElapsedMilliseconds
