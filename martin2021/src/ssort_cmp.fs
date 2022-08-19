let rec sel lt m ys =
  function [] -> (m,ys)
         | x::xs -> if lt x m then sel lt x (m::ys) xs
                    else sel lt m (x::ys) xs

let rec ssort (lt :'a->'a->bool) : 'a list -> 'a list =
  function [] -> []
         | x::xs -> let (m,xs) = sel lt x [] xs
                    in m :: ssort lt xs

do printfn "%A" (ssort (>) ["Dog"; "Apple"; "Horse"; "Monkey"])

do printfn "%A" (ssort (<) ["Dog"; "Apple"; "Horse"; "Monkey"])
