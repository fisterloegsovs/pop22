let rec find p xs =
  if List.isEmpty xs then None
  else if p (List.head xs) then Some (List.head xs)
       else find p (List.tail xs)

let xs = [34;23;56;76;23]

do printf "%A\n" (find (fun x -> x > 50) xs)
