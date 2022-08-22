open System
let rec loop (a:float) : float =
  match Console.ReadLine() with
    | "" -> a
    | s -> loop (a+float(s))
do Console.WriteLine "Enter numbers (end with empty line):"
do try printfn "Sum: %f" (loop 0.0) with
     | _ -> Console.Error.WriteLine "Expecting numbers"
