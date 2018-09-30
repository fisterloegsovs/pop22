let N = 100000
let arr = [|1..N|];;
let mutable max = -1
for i = 0 to arr.Length - 1 do
  let v = arr.[i];
  max <- if v > max then v else max
printfn "%A" max
