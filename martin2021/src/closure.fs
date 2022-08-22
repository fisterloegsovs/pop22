let rec loop i : (int->int) list =
  if i < 0 then []
  else (fun x -> i * x) :: loop (i-1)   // i is caught here!

let fs = loop 200

let xs = List.map (fun f -> f 3) fs

do printfn "%A" xs
