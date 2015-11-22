let rec fact = function
  | 0 -> 1
  | x -> x*(fact (x-1))

let impFact x =
  let mutable prod = 1;
  let mutable xn = x;
  while xn > 0 do
    prod <- prod*xn
    xn <- xn - 1
  prod

let duration f = 
    let timer = new System.Diagnostics.Stopwatch ()
    timer.Start ()
    let returnValue = f ()
    let dur = ((float timer.ElapsedTicks) / (float System.Diagnostics.Stopwatch.Frequency))
    (returnValue, dur)

let (factN, durFactN) = duration (fun () -> fact 10)
let (impFactN, durImpFactN) = duration (fun () -> impFact 10)
printfn "fact 10 = %d (%g sec)" factN durFactN
printfn "impFact 10 = %d (%g sec, %g speedup)" impFactN durImpFactN (durFactN / durImpFactN)
