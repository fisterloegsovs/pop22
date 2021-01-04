open System
let fahrenheit (c:float) : unit = 
  if c < -273.15 then failwith "input too small" 
  else printfn "Fahrenheit: %f" (9.0/5.0*c + 32.0) 

Console.Write "Temperature in degrees Celcius: " 
let s = Console.ReadLine()
try fahrenheit(float(s)) with 
  | Failure s -> Console.Error.WriteLine s 
  | _ -> Console.Error.WriteLine "Expecting number" 
