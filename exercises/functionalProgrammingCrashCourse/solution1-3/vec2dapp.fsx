
let pi = 3.141592

let polyLen (n : int) : float = 
  let theta i n = 2.0 * pi * (float i)/(float n)
  let mutable sum = 0.0;
  for i = 0 to n do
    let t1 = theta i n
    let t2 = theta (i+1) n
    let v = (cos t2 - cos t1, sin t2 - sin t1)
    sum <- sum + vec2d.len v
  sum

let n = 10
printfn "polyLen %d = %f" n (polyLen n)
printfn "       n  polyLen     Error"
for i = 1 to 5 do
  let n = pown 10 i
  let est = polyLen n
  let err = abs (2.0 * pi - est)
  printfn "% 8d % 8.4f % 8.1e" n est err
