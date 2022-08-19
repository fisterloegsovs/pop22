let rec splitAlt (xs: int list) : int list * int list =
  match xs with
    | [] -> [], []
    | [x] -> [x], []
    | x::y::zs ->
      let xs, ys = splitAlt zs
      in (x::xs),(y::ys)

let split xs =
  let xs,ys,_ = List.foldBack (fun x (xs,ys,even) ->
                               if even then (x::xs,ys,false)
                               else (xs,x::ys,true)) xs ([],[],true)
  in xs,ys

let v = split [1;2;3;4;5;6;7;8;9]

let res = v = ([1;3;5;7;9],[2;4;6;8])
do printf "%b\n" res
