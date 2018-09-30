let N = 100000
let lst = [1..N];;
let mutable max = -1
for i = 0 to lst.Length - 1 do
  let v = lst.[i];
  max <- if v > max then v else max
printfn "%A" max
