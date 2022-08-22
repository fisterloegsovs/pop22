let rec sel lt m ys =
  function [] -> (m,ys)
         | x::xs -> if lt x m then
                      sel lt x (m::ys) xs
                    else sel lt m (x::ys) xs

let rec ssort lt =
  function [] -> []
         | x::xs ->
           let (m,xs) = sel lt x [] xs
           in m :: ssort lt xs

let t1 = [34;7;34;23;2;73;2;36;86;12;11;4;54;35]

let t2 = List.map (fun x -> string(x)) t1

do printf "%A\n" t1

do printf "%A\n" (ssort (<) t1)

do printf "%A\n" (ssort (>) t2)
