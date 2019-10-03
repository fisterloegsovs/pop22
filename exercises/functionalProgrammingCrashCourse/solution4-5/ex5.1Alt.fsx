let rec multiplicity x xs =
  List.fold (fun acc elm -> acc + (if elm = x then 1 else 0)) 0 xs

let lst = [1; 2; 1; 3; 5; 3; 1]
for i = 1 to 5 do
   printfn "multiplicity %d %A = %d" i lst (multiplicity i lst)
