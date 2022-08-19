
// Maximum segment sum

let rec mess (a:int array) i =
  if i < 0 then 0
  else max (a.[i]) (mess a (i-1) + a.[i])

let rec mss (a:int array) i =
  if i < 0 then 0
  else max (mess a i) (mss a (i-1))

let ex = [|-2; 1; -3; 4; -1; 2; 1; -5; 4|]

do printfn "%A" (mss ex (ex.Length-1))
do printfn "mess(ex)(4)=%A" (mess ex 4)
do printfn "mss(ex)(4)=%A" (mss ex 4)
