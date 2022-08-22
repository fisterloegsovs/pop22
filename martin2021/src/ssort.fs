let rec select (xs:int list) (m,ys) =
  match xs with
    | [] -> (m,ys)
    | x::xs -> if x < m then select xs (x,m::ys)
               else select xs (m,x::ys)

let rec ssort xs =
  match xs with
    | [] -> []
    | x::xs -> let (m,xs) = select xs (x,[])
               in m::ssort xs

let t1 = [34;7;34;23;2;73;2;36;86;12;11;4;54;35]

do printf "%A\n" t1

do printf "%A\n" (ssort t1)
