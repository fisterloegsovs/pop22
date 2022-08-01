let rec multiplicity x xs =
  match xs with
    elm :: rest -> 
      let v = if (x = elm) then 1 else 0
      v + multiplicity x rest
    | [] -> 0

let lst = [1; 2; 1; 3; 5; 3; 1]
for i = 1 to 5 do
   printfn "multiplicity %d %A = %d" i lst (multiplicity i lst)
