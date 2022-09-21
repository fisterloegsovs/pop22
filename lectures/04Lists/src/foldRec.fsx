// List.fold: f:('S -> 'T -> 'S) -> acc:'S -> lst:'T list -> 'S

let rec fold (f: 'S -> 'T -> 'S) (acc:'S) (lst:'T list) : 'S  =
  match lst with
    [] -> acc
    | elm::rst -> fold f (f acc elm) rst

let lst = fold (fun acc i -> (i*i)::acc) [] [0..3]
