// List.map: f:('T -> 'U) -> lst:'T list -> 'U list

let rec map (f: 'T -> 'U) (lst: 'T list) : 'U list =
  match lst with
    [] -> []
    | elm::rst -> f elm :: map f rst

let lst = map (fun i -> i*i) [0..3]
