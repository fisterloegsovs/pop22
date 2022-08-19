let gennemsnit (xs: float list) : float option =
  if xs.Length <= 0 then None
  else Some (List.fold (fun acc x -> acc+x) 0.0 xs / float xs.Length)

let res = gennemsnit [4.0;5.0;3.0;8.0] = Some 5.0
do printf "%b\n" res
