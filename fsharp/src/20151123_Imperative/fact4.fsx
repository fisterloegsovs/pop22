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

let mutable n = Unchecked.defaultof<int>;
let rnd = System.Random()
let mutable counter = 10;
while counter > 0 do
  n <- rnd.Next(2, 10)
  counter <- counter - 1
  let (factN, durFactN) = duration (fun () -> fact n)
  let (impFactN, durImpFactN) = duration (fun () -> impFact n)
  printfn "fact %d = %d (%g sec)" n factN durFactN
  printfn "impFact %d = %d (%g sec, %g speedup)" n impFactN durImpFactN (durFactN / durImpFactN)
