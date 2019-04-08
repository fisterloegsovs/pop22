let mulByConst  (a:float) (p:float [])= Array.map (fun e -> e*a) p
let mulByX (p:float []) = Array.append [|0.0|] p
let rec add (p:float []) (q:float []) =
  if p.Length > q.Length then
    add q p
  else // q is equal or longer in size than p
    if p.Length = 0 then
      q
    else
      let n = Array.create q.Length 0.0
      for i = 0 to p.Length - 1 do
        n.[i] <- p.[i] + q.[i]
      for i = p.Length to q.Length - 1 do
        n.[i] <- q.[i]
      n
let rec mul (p:float []) (q:float []) =
  if p.Length = 0 || q.Length = 0 then
    [||]
  else
    add (mulByConst p.[0] q) (mulByX (mul p.[1..] q))

let p = [|1.0; 3.2; 0.0; -2.0|]
let q = [|2.0; 1.3|]
printfn "p = %A" p
printfn "q = %A" q
printfn "3.0 p = %A" (mulByConst 3.0 p)
printfn "x p = %A" (mulByX p)
printfn "p + q = %A" (add p q)
printfn "p q = %A" (mul p q)
