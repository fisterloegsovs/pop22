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
    let ticks = timer.ElapsedTicks
    let dur = ((float ticks) / (float System.Diagnostics.Stopwatch.Frequency))
    (returnValue, dur)

let (factN, durFactN) = duration (fun () -> fact 2)
let (impFactN, durImpFactN) = duration (fun () -> impFact 2)
printfn "fact 2 = %d (%g sec)" factN durFactN
printfn "impFact 2 = %d (%g sec, %g speedup)" impFactN durImpFactN (durFactN / durImpFactN)
