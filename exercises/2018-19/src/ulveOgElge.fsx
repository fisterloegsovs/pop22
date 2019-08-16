let e = 10
let u = 5
let n = 10
let fe = 20
let fu = 20
let s = 5
let T = 10
let isle = animals.environment(n,e, fe, u, fu, s)

for t = 1 to T do
  printfn "%A" isle
  isle.tick ()
  
